{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
module CompareRevisions.Git
  ( URL(..)
  , toText
  , Branch(..)
  , RevSpec(..)
  , Revision(..)
  , GitError(..)
  , ensureCheckout
  , syncRepo
  , getLog
  -- * Exported for testing purposes
  , runGit
  , runGitInRepo
  ) where

import Protolude

import qualified Control.Logging as Log
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Text as Text
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
newtype Hash = Hash Text deriving (Eq, Ord, Show, Generic, FromJSON)

-- | Specifies a revision in a Git repository.
newtype RevSpec = RevSpec Text deriving (Eq, Ord, Show, Generic, FromJSON)

-- | A Git revision.
data Revision
  = Revision
  { abbrevHash :: Text  -- TODO: Hash
  , commitDate :: Text  -- TODO: some sort of data type
  , authorName :: Text
  , subject :: Text
  } deriving (Eq, Ord, Show)

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


-- | Ensure a checkout exists at the given path.
--
-- Assumes that:
--   * we have write access to the repo (we create checkouts under there)
--   * we are responsible for managing the checkout path
--
-- Checkout path is a symlink to the canonical location of the working tree,
-- which is updated to point at a new directory if they are out of date.
ensureCheckout
  :: (MonadError GitError m, MonadIO m, HasCallStack)
  => FilePath -- ^ Path to a Git repository on disk
  -> Branch -- ^ The branch we want to check out
  -> FilePath -- ^ The path to the checkout
  -> m ()
ensureCheckout repoPath branch workTreePath = do
  Log.debug' $ "Ensuring checkout of " <> toS repoPath <> " to " <> show branch <> " at " <> toS workTreePath
  hash@(Hash hashText) <- hashForBranchHead branch
  let canonicalTree = repoPath </> ("rev-" <> toS hashText)
  addWorkTree canonicalTree hash
  oldTree <- liftIO $ swapSymlink workTreePath canonicalTree
  case oldTree of
    Nothing -> pass
    Just oldTreePath
      | oldTreePath == canonicalTree -> pass
      | otherwise -> do
          Log.debug' $ "Removing old working tree: " <> toS oldTreePath
          removeWorkTree oldTreePath

  where
    -- | Get the SHA-1 of the head of a branch.
    hashForBranchHead :: (HasCallStack, MonadError GitError m, MonadIO m) => Branch -> m Hash
    hashForBranchHead (Branch b) = Hash . Text.strip . toS . fst <$> runGitInRepo repoPath ["rev-list", "-n1", b]

    -- | Checkout a branch of a repo to a given path.
    addWorkTree :: (HasCallStack, MonadIO m, MonadError GitError m) => FilePath -> Hash -> m ()
    addWorkTree path (Hash hash) =
      -- TODO: Doesn't handle case where path exists but is a file (not a
      -- directory), or doesn't contain a valid worktree.
      unlessM (liftIO $ fileExist path) $ do
        void $ runGitInRepo repoPath ["worktree", "add", toS path, hash]
        Log.debug' $ "Added work tree at " <> toS path

    removeWorkTree path = do
      liftIO $ unlessM (fileExist path) $ removeDirectoryRecursive path
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

getLog :: (MonadError GitError m, MonadIO m) => FilePath -> RevSpec -> RevSpec -> Maybe [FilePath] -> m [Revision]
getLog repoPath (RevSpec start) (RevSpec end) paths = do
  let command = ["log", "--first-parent", "--format=%h::%cd::%an::%s",  "--date=iso", range]
  let withFilter = command <> case paths of
                                Nothing -> []
                                Just ps -> ["--"] <> map toS ps
  (out, _) <- runGitInRepo repoPath withFilter
  -- XXX: This will bork on the first invalid line. We can do better.
  traverse parseRevision (ByteString.lines out)
  where
    range = start <> ".." <> end
    parseRevision line =
      case Text.splitOn "::" (decodeUtf8 line) of
        [hash, date, name, subject] -> pure (Revision hash date name subject)
        _ -> throwError (InvalidRevision line)

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
