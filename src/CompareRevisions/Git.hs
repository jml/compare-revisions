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
  , firstCommitSince
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
-- Does /not/ use the same algorithm as Git. Instead naively gets the last 8
-- characters of the full hash.
abbrevHash :: Revision -> Text
abbrevHash = Text.takeEnd 8 . unHash . revisionHash

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
  => URL -- ^ URL of Git repository to synchronize
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
      fetchRepo repoPath
    else do
      Log.debug' "Downloading new repo"
      cloneRepo url repoPath
  Log.debug' "Repo updated"

-- | Clone a Git repository.
cloneRepo :: (HasCallStack, MonadError GitError m, MonadIO m) => URL -> FilePath -> m ()
cloneRepo url gitRoot = void $ runGit (["clone", "--mirror"] <> [toText url, toS gitRoot])

-- | Fetch the latest changes to a Git repository.
fetchRepo :: (HasCallStack, MonadIO m, MonadError GitError m) => FilePath -> m ()
fetchRepo repoPath = void $ runGitInRepo repoPath ["fetch", "--all", "--prune"]


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
    hashForBranchHead (Branch b) = Hash . Text.strip . toS . fst <$> runGitInRepo repoPath ["rev-list", "-n1", b]


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
        void $ runGitInRepo repoPath ["worktree", "add", toS path, hashText]
        Log.debug' $ "Added work tree at " <> toS path

    removeWorkTree path = do
      void $ liftIO $ tryJust (guard . isDoesNotExistError) (removeDirectoryRecursive path)
      void $ runGitInRepo repoPath ["worktree", "prune"]
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
  (out, _) <- runGitInRepo repoPath command
  pure $ Hash . toS <$> lastMay (ByteString.lines out)

-- | Format a UTC time in ISO format.
formatIsoTime :: Time.UTCTime -> Text
formatIsoTime = toS . Time.formatTime Time.defaultTimeLocale (Time.iso8601DateFormat (Just "%H:%M:%S %z"))

getLog :: (MonadError GitError m, MonadIO m) => FilePath -> RevSpec -> RevSpec -> Maybe [FilePath] -> m [Revision]
getLog repoPath (RevSpec start) (RevSpec end) paths = do
  let command = ["log", "--first-parent", "--format=fuller", "--date=iso", range]
  let withFilter = command <> case paths of
                                Nothing -> []
                                Just ps -> ["--"] <> map toS ps
  (out, _) <- runGitInRepo repoPath withFilter
  parseFullerRevisions out
  where
    range = start <> ".." <> end

-- | Run 'git' in a repository.
runGitInRepo :: (HasCallStack, MonadError GitError m, MonadIO m) => FilePath -> [Text] -> m (ByteString, ByteString)
runGitInRepo repoPath args = runProcess $ gitCommand (Just repoPath) args

-- | Run 'git' on the command line.
runGit :: (HasCallStack, MonadError GitError m, MonadIO m) => [Text] -> m (ByteString, ByteString)
runGit args = runProcess $ gitCommand Nothing args

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
gitCommand :: Maybe FilePath -> [Text] -> CreateProcess
gitCommand repoPath args = (proc "git" (map toS args)) { cwd = repoPath }
