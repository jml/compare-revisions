{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
module CompareRevisions.Git
  ( Url(..)
  , GitError(..)
  , ensureCheckout
  , syncRepo
  ) where

import Protolude

import qualified Data.Text as Text
import Data.Yaml (FromJSON)
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
  , readCreateProcessWithExitCode
  , showCommandForUser
  )

-- | The URL to a Git repository.
newtype Url = Url Text deriving (Eq, Ord, Show, Generic, FromJSON)

-- | A Git branch.
newtype Branch = Branch Text deriving (Eq, Ord, Show, Generic, FromJSON)

-- | A SHA-1 hash for a Git revision.
newtype Hash = Hash Text deriving (Eq, Ord, Show, Generic, FromJSON)

-- | The human specification for a Git revision. e.g. 'HEAD', 'origin/master'.
newtype RevSpec = RevSpec Text deriving (Eq, Ord, Show)

-- XXX: Not sure this is a good idea. Maybe use exceptions all the way
-- through?
-- | An error that occurs while we're doing stuff.
data GitError
  -- | An error occurred running the 'git' subprocess.
  = GitProcessError Text Int Text Text (Maybe FilePath)
  deriving (Eq, Show)

-- | Sync a repository.
--
-- If the repository does not exist locally, it will be cloned from the URL.
-- If it does, it will be updated.
syncRepo
  :: (MonadIO m, MonadError GitError m, HasCallStack)
  => Url -- ^ URL of Git repository to synchronize
  -> FilePath -- ^ Where to store the bare Git repository
  -> m ()
syncRepo url repoPath = do
  repoExists <- liftIO $ fileExist repoPath
  if repoExists
    then fetchRepo repoPath
    else cloneRepo url repoPath

-- | Clone a Git repository.
cloneRepo :: (HasCallStack, MonadError GitError m, MonadIO m) => Url -> FilePath -> m ()
cloneRepo (Url url) gitRoot = void $ runGit (["clone", "--mirror"] <> [url, toS gitRoot])

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
  -> RevSpec -- ^ The revision of the Git repository we want to check out
  -> FilePath -- ^ The path to the checkout
  -> m ()
ensureCheckout repoPath revSpec workTreePath = do
  hash@(Hash hashText) <- hashForRev revSpec
  let canonicalTree = repoPath </> ("rev-" <> toS hashText)
  -- XXX: What happens if we already have a checkout?! If the current checkout
  -- already *is* this checkout?
  addWorkTree canonicalTree hash
  oldTree <- liftIO $ swapSymlink workTreePath canonicalTree
  for_ oldTree removeWorkTree

  where
    -- | Get the SHA-1 of a revision.
    hashForRev :: (HasCallStack, MonadError GitError m, MonadIO m) => RevSpec -> m Hash
    hashForRev (RevSpec rev) = Hash . Text.strip . fst <$> runGitInRepo repoPath ["rev-list", "-n1", rev]

    -- | Checkout a branch of a repo to a given path.
    addWorkTree :: (HasCallStack, MonadIO m, MonadError GitError m) => FilePath -> Hash -> m ()
    addWorkTree path (Hash hash) = do
      void $ runGitInRepo repoPath ["worktree", "add", toS path, hash]
      -- XXX: See comment in swapSymlink
      let relativeWorkingPath = makeRelative repoPath (toS path)
      let gitDirRef = "gitdir: " <> ("../worktrees" </> relativeWorkingPath) <> "\n"
      liftIO $ writeFile (toS workTreePath </> ".git") (toS gitDirRef)

    removeWorkTree path = do
      liftIO $ removeDirectoryRecursive path
      void $ runGitInRepo repoPath ["worktree", "prune"]

    -- | Ensure the symlink at 'linkPath' points to 'newPath'. Return the target
    -- of the old path.
    swapSymlink :: HasCallStack => FilePath -> FilePath -> IO (Maybe FilePath)
    swapSymlink linkPath newPath = do
      currentPath <- getSymlink linkPath
      let base = takeDirectory linkPath
      -- TODO: 'makeRelative' will never return paths with '..', in a noble
      -- attempt to prevent us shooting ourselves in the foot. (c.f.
      -- http://neilmitchell.blogspot.co.uk/2015/10/filepaths-are-subtle-symlinks-are-hard.html)
      -- However, we need a relative link, because the volume that we're doing
      -- this on might be mounted at a different path on another container.
      let newPathRelative = makeRelative base newPath
      -- TODO: Handle tmp-link existing
      createSymbolicLink newPathRelative (base </> "tmp-link")
      rename (base </> "tmp-link") linkPath
      pure currentPath

    getSymlink :: HasCallStack => FilePath -> IO (Maybe FilePath)
    getSymlink path = do
      result <- tryJust (guard . isDoesNotExistError) (readSymbolicLink path)
      pure $ hush result

-- | Run 'git' in a repository.
runGitInRepo :: (HasCallStack, MonadError GitError m, MonadIO m) => FilePath -> [Text] -> m (Text, Text)
runGitInRepo repoPath args = runProcess $ gitCommand (Just repoPath) args

-- | Run 'git' on the command line.
runGit :: (HasCallStack, MonadError GitError m, MonadIO m) => [Text] -> m (Text, Text)
runGit args = runProcess $ gitCommand Nothing args

-- | Run a process.
runProcess :: (HasCallStack, MonadError GitError m, MonadIO m) => CreateProcess -> m (Text, Text)
runProcess process = do
  -- TODO: logging
  (exitCode, out, err) <- liftIO $ readCreateProcessWithExitCode process ""
  let out' = toS out
  let err' = toS err
  case exitCode of
    ExitFailure e -> throwError $ GitProcessError (toS cmdInfo) e out' err' (cwd process)
    ExitSuccess -> pure (out', err')
  where
    cmdInfo =
      case spec of
        ShellCommand s -> s
        RawCommand path args -> showCommandForUser path args
    spec = cmdspec process

-- | Get the CreateProcess for running git.
gitCommand :: HasCallStack => Maybe FilePath -> [Text] -> CreateProcess
gitCommand repoPath args = (proc "git" (map toS args)) { cwd = repoPath }
