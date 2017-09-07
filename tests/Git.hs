{-# LANGUAGE FlexibleContexts #-}
module Git (tests) where

import Protolude
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec, describe, it, shouldBe)

import qualified CompareRevisions.Git as Git
import CompareRevisions.Server.Logging (withLogging, LogLevel(..))

tests :: IO TestTree
tests = testSpec "Git" $
  describe "ensureCheckout" $ do
    it "checks out a repository" $ withLogging LevelError $ withSystemTempDirectory "base-directory" $ \baseDir -> do
      let repoDir = baseDir </> "repo"
      gitInit repoDir
      writeFile (repoDir </> "hello") "dummy content"
      git repoDir ["add", "hello"]
      git repoDir ["commit", "-m", "Initial commit"]
      let workingTreeDir = baseDir </> "working-tree"
      gitOp $ Git.ensureCheckout repoDir (Git.Branch "master") workingTreeDir
      contents <- readFile (workingTreeDir </> "hello")
      contents `shouldBe` "dummy content"
    it "updates existing checkouts" $ withLogging LevelError $ withSystemTempDirectory "base-directory" $ \baseDir -> do
      let repoDir = baseDir </> "repo"
      gitInit repoDir
      writeFile (repoDir </> "hello") "dummy content"
      git repoDir ["add", "hello"]
      git repoDir ["commit", "-m", "Initial commit"]
      let workingTreeDir = baseDir </> "working-tree"
      gitOp $ Git.ensureCheckout repoDir (Git.Branch "master") workingTreeDir
      writeFile (repoDir </> "hello") "different content"
      git repoDir ["add", "hello"]
      git repoDir ["commit", "-m", "Second commit"]
      gitOp $ Git.ensureCheckout repoDir (Git.Branch "master") workingTreeDir
      contents <- readFile (workingTreeDir </> "hello")
      contents `shouldBe` "different content"

git :: MonadIO m => FilePath -> [Text] -> m ()
git repo args = gitOp $ Git.runGitInRepo repo args

gitInit :: MonadIO m => FilePath -> m ()
gitInit repoDir = do
  gitOp $ Git.runGit ["init", toS repoDir]
  git repoDir ["config", "user.name", "testuser"]
  git repoDir ["config", "user.email", "testuser@example.com"]

gitOp :: Monad m => ExceptT Git.GitError m a -> m ()
gitOp op = do
  result <- runExceptT op
  case result of
    Left err -> panic $ "git operation failed: " <> show err
    -- Could return result but we don't need to. Just return () to keep tests succinct.
    Right _ -> pure ()
