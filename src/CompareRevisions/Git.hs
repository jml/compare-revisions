{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
module CompareRevisions.Git
  ( URL(..)
  , toText
  , Branch(..)
  , Hash(..)
  , RevSpec(..)
  , Revision(..)
  , abbrevHash
  , GitError(..)
  , ensureCheckout
  , ensureCheckout'
  , syncRepo
  , getLog
  , commitsInWindow
  , firstCommitSince
  , getRevisions
  -- * Credential management
  , Secret(..)
  , CredentialError(..)
  , URLWithCredentials(..)
  , toURL
  , applyCredentials
  -- * Exported for testing purposes
  , runGit
  , runGitInRepo
  , parseFullerRevisions
  ) where

import Protolude hiding (hash)

import qualified Control.Logging as Log
import qualified Data.Attoparsec.Text as Atto
import qualified Data.Attoparsec.Time as Atto
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Data.Time as Time
import Data.Aeson (FromJSON(..), ToJSON(..), withText)
import qualified Network.URI
import System.Directory (removeDirectoryRecursive)
import System.FilePath ((</>), makeRelative, takeDirectory)
import System.IO.Error (isDoesNotExistError)
import System.Posix.Files
  ( createSymbolicLink
  , fileExist
  , readSymbolicLink
  , rename
  )
import System.Process
  ( CmdSpec(..)
  , CreateProcess(..)
  , proc
  , showCommandForUser
  )
import System.Process.ByteString (readCreateProcessWithExitCode)

import CompareRevisions.SCP (SCP, formatSCP, parseSCP)
import qualified CompareRevisions.SCP as SCP


-- | The URL to a Git repository.
data URL
  = URI Network.URI.URI
  | SCP SCP
  deriving (Eq, Ord, Show, Generic)

toText :: URL -> Text
toText (URI uri) = toS $ Network.URI.uriToString identity uri ""
toText (SCP scp) = formatSCP scp

instance ToJSON URL where
  toJSON = toJSON . toText

instance FromJSON URL where
  parseJSON = withText "URI must be text" $ \text ->
    maybe empty pure (URI <$> Network.URI.parseAbsoluteURI (toS text)) <|> (SCP <$> parseSCP text)


-- | Credentials required to pull a Git repository.
--
-- Normally constructed via 'loadSecret'.
data Secret
  = -- | HTTP basic auth, intended to be used over HTTPS.
    BasicAuth
    { username :: Username
    , password :: Password
    }
  | -- | SSH key. Normally a GitHub deploy key.
    SSHKey
    { -- | The username to connect as via SSH. For GitHub, this is @git@.
      sshUsername :: SCP.Username
      -- | Path to a file containing the SSH private key. We never read this file.
    , sshKeyFile :: FilePath
    }
  deriving (Eq, Ord, Show)

-- | A username.
type Username = ByteString

-- | A password.
type Password = ByteString

-- | Everything we need to fetch a Git repository, including credentials.
data URLWithCredentials
  = -- | A URL, normally https, with a username and password set.
    CredURI Network.URI.URI
  | -- | A repository on local disk.
    LocalFile FilePath
  | -- | A repository on another computer, accessed via SSH.
    RemoteFile
    { -- | The user to SSH as.
      _credSSHUsername :: SCP.Username
    , -- | The path to the user's private key
      credSSHKeyFile :: FilePath
    , -- | The hostname to connect to
      _credSSHHostname :: SCP.Hostname
    , -- | The path of the repository on the host
      _credSSHFilePath :: FilePath
    }
  deriving (Eq, Ord, Show)

toURL :: URLWithCredentials -> URL
toURL (CredURI uri) = URI uri
toURL (LocalFile path) = SCP (SCP.File path)
toURL (RemoteFile user _ host path) = SCP (SCP.AuthRemoteFile user host path)

-- | Get the key file from the URL, if it has one.
getKeyFile :: URLWithCredentials -> Maybe FilePath
getKeyFile RemoteFile{credSSHKeyFile} = Just credSSHKeyFile
getKeyFile _ = Nothing

-- | An error that occurs while applying credentials to a URI.
data CredentialError
  = -- | It doesn't make sense to have a username and password in a URI without a host.
    NoHostInURI Username Network.URI.URI
  | -- | We don't support using SSH keys for authentication for URIs. Use SCP addresses instead.
    SSHKeyWithURI FilePath Network.URI.URI
  | -- | We don't support using SSH without explicit credentials.
    SSHWithoutCredentials SCP
  | -- | Using SSH credentials to access a local path doesn't make any sense.
    LocalPathWithCredentials FilePath Secret
  | -- | We don't support using a password for SSH access.
    PasswordForSSH SCP
  | -- | Not going to guess between different usernames.
    DifferentUsernames SCP SCP.Username
  | -- | Not going to guess between different sets of credentials.
    DifferentCredentials Network.URI.URI Username Password
  deriving (Eq, Ord, Show)

-- | Apply whatever credentials we might have been given for a URL to that URL,
-- creating a 'URLWithCredentials' that we can use to access the repository.
applyCredentials :: MonadError CredentialError e => URL -> Maybe Secret -> e URLWithCredentials
applyCredentials (URI uri)             Nothing                                         = pure (CredURI uri)
applyCredentials (URI uri)             (Just SSHKey {sshKeyFile})                      = throwError (SSHKeyWithURI sshKeyFile uri)
applyCredentials (URI uri)             (Just (BasicAuth username password))            =
  case Network.URI.uriAuthority uri of
    Nothing -> throwError (NoHostInURI username uri)  -- Doesn't make sense to have a password but no URI.
    Just authority ->
      let userInfo = toS ( username <> ":" <> password <> "@" )
      in case Network.URI.uriUserInfo authority of
           "" -> pure (CredURI (uri { Network.URI.uriAuthority = Just authority { Network.URI.uriUserInfo = userInfo } }))
           existing
             | existing == userInfo -> pure (CredURI uri)
             | otherwise -> throwError (DifferentCredentials uri username password)
applyCredentials (SCP (SCP.File path)) Nothing                                         = pure (LocalFile path) -- We can access local files without keys
applyCredentials (SCP (SCP.File path)) (Just secret)                                   = throwError (LocalPathWithCredentials path secret)
applyCredentials (SCP scp)             Nothing                                         = throwError (SSHWithoutCredentials scp) -- Anything remote needs a key
applyCredentials (SCP scp)             (Just BasicAuth {})                             = throwError (PasswordForSSH scp)
applyCredentials (SCP (SCP.RemoteFile hostname path)) (Just (SSHKey username keyfile)) =
  pure (RemoteFile username keyfile hostname path)
applyCredentials (SCP scp@(SCP.AuthRemoteFile scpName hostname path)) (Just (SSHKey keyUsername keyfile))
  | scpName == keyUsername = pure (RemoteFile scpName keyfile hostname path)
  | otherwise              = throwError (DifferentUsernames scp keyUsername)

-- | A Git branch.
newtype Branch = Branch Text deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

-- | A SHA-1 hash for a Git revision.
newtype Hash = Hash { unHash :: Text } deriving (Eq, Ord, Show, Generic, FromJSON)

-- | Parse a full SHA1 hash.
fullHashParser :: Atto.Parser Hash
fullHashParser = Hash . toS <$> Atto.takeWhile1 Char.isHexDigit

-- | Specifies a revision in a Git repository.
newtype RevSpec = RevSpec Text deriving (Eq, Ord, Show, Generic, FromJSON)

-- | A Git revision.
data Revision
  = Revision
  { revisionHash :: Hash
  , commitDate :: Time.UTCTime
  , authorName :: Text
  , subject :: Text
  , body :: Maybe Text
  } deriving (Eq, Ord, Show)

-- | Get the abbreviated hash for a revision.
--
-- Does /not/ use the same algorithm as Git. Instead naively gets the first 8
-- characters of the full hash.
abbrevHash :: Revision -> Text
abbrevHash = Text.take 8 . unHash . revisionHash

-- | Parser for a "fuller" Git revision.
--
-- e.g.
--
-- commit 2c141409774c95bb0f8e6db9773c50de8c4dc6ea
-- Author:     bryan <bryan@weave.works>
-- AuthorDate: 2018-03-27 13:08:24 +0000
-- Commit:     Weave Flux <support@weave.works>
-- CommitDate: 2018-03-27 13:08:24 +0000
--
--     I want to leave a bit longer for cortex#681 to be tested in dev
--
--     - Locked: cortex:deployment/distributor
--     - Updated policies: cortex:deployment/distributor
fullerRevisionParser :: Atto.Parser Revision
fullerRevisionParser = do
  -- We're ignoring a lot of information here (the underscore-prefixed
  -- variables). This is because we don't have actual use-cases for it yet.
  -- Please feel free to store this information when we do need it.
  fullHash <- "commit " *> fullHashParser <* Atto.endOfLine
  _merge <- optional ("Merge: " *> (abbrevHashParser `Atto.sepBy` " ") <* Atto.endOfLine)
  (authorName, _authorEmail) <- "Author:     " *> personParser <* Atto.endOfLine
  _authorDate <- "AuthorDate: " *> Atto.utcTime <* Atto.endOfLine
  (_committerName, _committerEmail) <- "Commit:    " *> personParser <* Atto.endOfLine
  commitDate <- "CommitDate: " *> Atto.utcTime <* Atto.endOfLine
  Atto.endOfLine
  subject <- commitMessageLine
  body <- optional ("    " *> Atto.endOfLine *> (Text.unlines <$> many commitMessageLine))
  pure $ Revision fullHash commitDate authorName (toS subject) body
  where
    abbrevHashParser = Atto.takeWhile1 Char.isHexDigit
    personParser = do
      name <- Atto.takeTill (== '<')
      email <- Atto.takeTill (== '>')
      void $ Atto.char '>'
      pure (Text.strip name, email)
    commitMessageLine = "    " *> Atto.takeTill Atto.isEndOfLine <* Atto.endOfLine

-- | Parse the output of `git log --format=fuller`.
parseFullerRevisions :: MonadError GitError m => ByteString -> m [Revision]
parseFullerRevisions input =
  case Atto.parseOnly (fullerRevisionParser `Atto.sepBy` Atto.endOfLine) (toS input) of
    Left err -> throwError $ InvalidRevision (toS err)
    Right revs -> pure revs

-- XXX: Not sure this is a good idea. Maybe use exceptions all the way
-- through?
-- | An error that occurs while we're doing stuff.
data GitError
  -- | An error occurred running the 'git' subprocess.
  = GitProcessError Text Int ByteString ByteString (Maybe FilePath)
  | InvalidRevision ByteString
  deriving (Eq, Show)

-- | Sync a repository.
--
-- If the repository does not exist locally, it will be cloned from the URL.
-- If it does, it will be updated.
syncRepo
  :: (MonadIO m, MonadError GitError m, HasCallStack)
  => URLWithCredentials -- ^ URL of Git repository to synchronize
  -> FilePath -- ^ Where to store the bare Git repository
  -> m ()
syncRepo url repoPath = do
  Log.debug' $ "Syncing " <> show url <> " to " <> show repoPath
  repoExists <- liftIO $ fileExist repoPath
  if repoExists
    then do
      Log.debug' "Update existing repo"
      -- TODO: Wrongly assumes 'origin' is the same, i.e. that URL doesn't
      -- change. We work around this by using 'getRepoPath' to provide the
      -- path, which includes a hash of the URL.
      fetchRepo url repoPath
    else do
      Log.debug' "Downloading new repo"
      cloneRepo url repoPath
  Log.debug' "Repo updated"

-- | Clone a Git repository.
cloneRepo :: (HasCallStack, MonadError GitError m, MonadIO m) => URLWithCredentials -> FilePath -> m ()
cloneRepo url gitRoot = void $ runGit (getKeyFile url) (["clone", "--mirror"] <> [toText (toURL url), toS gitRoot])

-- | Fetch the latest changes to a Git repository.
fetchRepo :: (HasCallStack, MonadIO m, MonadError GitError m) => URLWithCredentials -> FilePath -> m ()
fetchRepo url repoPath = void $ runGitInRepo (getKeyFile url) repoPath ["fetch", "--all", "--prune"]


-- | Ensure a checkout of a branch exists at the given path.
ensureCheckout
  :: (MonadError GitError m, MonadIO m, HasCallStack)
  => FilePath -- ^ Path to a Git repository on disk
  -> Branch -- ^ The branch we want to check out
  -> FilePath -- ^ The path to the checkout
  -> m ()
ensureCheckout repoPath branch workTreePath = do
  Log.debug' $ "Ensuring checkout of " <> toS repoPath <> " to " <> show branch <> " at " <> toS workTreePath
  hash <- hashForBranchHead branch
  ensureCheckout' repoPath hash workTreePath
  where
    -- | Get the SHA-1 of the head of a branch.
    hashForBranchHead :: (HasCallStack, MonadError GitError m, MonadIO m) => Branch -> m Hash
    hashForBranchHead (Branch b) = Hash . Text.strip . toS . fst <$> runGitInRepo Nothing repoPath ["rev-list", "-n1", b]


-- | Ensure a checkout of the given revision exists at the given path.
--
-- Assumes that:
--   * we have write access to the repo (we create checkouts under there)
--   * we are responsible for managing the checkout path
--
-- Checkout path is a symlink to the canonical location of the working tree,
-- which is updated to point at a new directory if they are out of date.
ensureCheckout'
  :: (MonadError GitError m, MonadIO m, HasCallStack)
  => FilePath -- ^ Path to a Git repository on disk
  -> Hash -- ^ The Git SHA1 that we want to check out
  -> FilePath -- ^ The path to the checkout
  -> m ()
ensureCheckout' repoPath (Hash hashText) workTreePath = do
  let canonicalTree = repoPath </> ("rev-" <> toS hashText)
  addWorkTree canonicalTree
  oldTree <- liftIO $ swapSymlink workTreePath canonicalTree
  case oldTree of
    Nothing -> pass
    Just oldTreePath
      | oldTreePath == canonicalTree -> pass
      | otherwise -> do
          Log.debug' $ "Removing old working tree: " <> toS oldTreePath
          removeWorkTree oldTreePath

  where
    -- | Checkout a branch of a repo to a given path.
    addWorkTree :: (HasCallStack, MonadIO m, MonadError GitError m) => FilePath -> m ()
    addWorkTree path =
      -- TODO: Doesn't handle case where path exists but is a file (not a
      -- directory), or doesn't contain a valid worktree.
      unlessM (liftIO $ fileExist path) $ do
        void $ runGitInRepo Nothing repoPath ["worktree", "add", toS path, hashText]
        Log.debug' $ "Added work tree at " <> toS path

    removeWorkTree path = do
      void $ liftIO $ tryJust (guard . isDoesNotExistError) (removeDirectoryRecursive path)
      void $ runGitInRepo Nothing repoPath ["worktree", "prune"]
      Log.debug' $ "Removed worktree from " <> toS path

    -- | Ensure the symlink at 'linkPath' points to 'newPath'. Return the target
    -- of the old path if it differs from the new path.
    swapSymlink :: HasCallStack => FilePath -> FilePath -> IO (Maybe FilePath)
    swapSymlink linkPath newPath = do
      Log.debug' $ "Updating symlink " <> toS linkPath <> " to point to " <> toS newPath
      currentPath <- getSymlink linkPath
      Log.debug' $ "Symlink currently points to: " <> show currentPath
      let base = takeDirectory linkPath
      let newPathRelative = makeRelative base newPath
      if Just newPathRelative == currentPath
        then pure Nothing
        else
        do let tmpLink = base </> "tmp-link"
           -- TODO: Handle tmp-link existing, or better yet, make it somewhere
           -- completely different.
           Log.debug' $ "Creating new link to " <> toS newPathRelative <> " at " <> toS tmpLink
           createSymbolicLink newPathRelative tmpLink
           Log.debug' $ "Renaming " <> toS tmpLink <> " to " <> toS linkPath
           rename (base </> "tmp-link") linkPath
           Log.debug' $ "Swapped symlink: " <> toS linkPath <> " now points to " <> toS newPath
           pure $ case currentPath of
             Nothing -> Nothing
             Just p -> Just (base </> p)

    getSymlink :: HasCallStack => FilePath -> IO (Maybe FilePath)
    getSymlink path = do
      result <- tryJust (guard . isDoesNotExistError) (readSymbolicLink path)
      pure $ hush result

-- | Find the first commit made since the given time, if there is such a commit.
firstCommitSince
  :: (HasCallStack, MonadError GitError m, MonadIO m)
  => FilePath  -- ^ Path to the repository
  -> Branch -- ^ The branch to list
  -> Time.UTCTime  -- ^ Earliest possible time of the commit.
  -> m (Maybe Hash)  -- ^ The hash of the commit.
firstCommitSince repoPath (Branch branch) time = do
  -- This approach means that if 'time' is a long time in the past, we'll have
  -- to process a *lot* of revisions. If this turns out to be a problem in
  -- practice, could either come up with a cleverer way of querying Git
  -- (preferred) or add a --before clause and exponentially back off until we
  -- reach a time greater than now.
  let command = ["rev-list", "--first-parent", "--after=" <> formatIsoTime time, branch]
  (out, _) <- runGitInRepo Nothing repoPath command
  pure $ Hash . toS <$> lastMay (ByteString.lines out)

-- | Find all the mainline commits that occurred between the two dates.
commitsInWindow
  :: (HasCallStack, MonadError GitError m, MonadIO m)
  => FilePath  -- ^ The repository's location on disk
  -> Branch  -- ^ The branch to query
  -> Time.UTCTime  -- ^ The start date
  -> Time.UTCTime  -- ^ The end date
  -> Maybe [FilePath]  -- ^ The paths within the repository we care about
  -> m [Revision]
commitsInWindow repoPath (Branch branch) startTime endTime paths = do
  let command = [ "log"
                , "--first-parent"
                , "--format=fuller"
                , "--date=iso"
                , "--after=" <> formatIsoTime startTime
                , "--before=" <> formatIsoTime endTime
                , branch
                ]
  let withFilter = command <> maybe [] (\ps -> ["--"] <> map toS ps) paths
  (out, _) <- runGitInRepo Nothing repoPath withFilter
  parseFullerRevisions out

-- | Load a bunch of revisions from a Git repository.
getRevisions :: (MonadError GitError m, MonadIO m) => FilePath -> [RevSpec] -> m [Revision]
getRevisions repoPath revSpecs = do
  let command = ["show", "-s", "--format=fuller", "--date=iso"] <> [spec | RevSpec spec <- revSpecs]
  (out, _) <- runGitInRepo Nothing repoPath command
  parseFullerRevisions out

-- | Format a UTC time in ISO format.
formatIsoTime :: Time.UTCTime -> Text
formatIsoTime = toS . Time.formatTime Time.defaultTimeLocale (Time.iso8601DateFormat (Just "%H:%M:%S %z"))

getLog :: (MonadError GitError m, MonadIO m) => FilePath -> RevSpec -> RevSpec -> Maybe [FilePath] -> m [Revision]
getLog repoPath (RevSpec start) (RevSpec end) paths = do
  let command = ["log", "--first-parent", "--format=fuller", "--date=iso", range]
  let withFilter = command <> case paths of
                                Nothing -> []
                                Just ps -> ["--"] <> map toS ps
  (out, _) <- runGitInRepo Nothing repoPath withFilter
  parseFullerRevisions out
  where
    range = start <> ".." <> end

-- | Run 'git' in a repository.
runGitInRepo :: (HasCallStack, MonadError GitError m, MonadIO m) => Maybe FilePath -> FilePath -> [Text] -> m (ByteString, ByteString)
runGitInRepo keyFile repoPath args = runProcess $ gitCommand keyFile (Just repoPath) args

-- | Run 'git' on the command line.
runGit :: (HasCallStack, MonadError GitError m, MonadIO m) => Maybe FilePath -> [Text] -> m (ByteString, ByteString)
runGit keyFile args = runProcess $ gitCommand keyFile Nothing args

-- | Run a process.
runProcess :: (HasCallStack, MonadError GitError m, MonadIO m) => CreateProcess -> m (ByteString, ByteString)
runProcess process = do
  Log.debug' $ "Running process: " <> toS cmdInfo <> "; " <> show process
  (exitCode, out, err) <- liftIO $ readCreateProcessWithExitCode process ""
  case exitCode of
    ExitFailure e -> do
      Log.warn' $ "Process failed (" <> show e <> "): " <> toS cmdInfo
      throwError $ GitProcessError (toS cmdInfo) e out err (cwd process)
    ExitSuccess -> do
      Log.debug' $ "Process succeeded: " <> toS cmdInfo
      pure (out, err)
  where
    cmdInfo =
      case spec of
        ShellCommand s -> s
        RawCommand path args -> showCommandForUser path args
    spec = cmdspec process

-- | Get the CreateProcess for running git.
gitCommand :: Maybe FilePath -> Maybe FilePath -> [Text] -> CreateProcess
gitCommand Nothing repoPath args
  = (proc "git" (map toS args)) { cwd = repoPath
                                , env = Just []  -- XXX: Probably not going to work due to git assuming presence of environment variables (e.g. USER, HOME).
                                }
gitCommand (Just keyFile) repoPath args
  = (proc "git" (map toS args)) { cwd = repoPath
                                , env = Just [ ("GIT_SSH_COMMAND", showCommandForUser "ssh" ["-i", keyFile]) ]
                                }
