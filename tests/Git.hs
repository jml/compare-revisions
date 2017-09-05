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
  describe "ensureCheckout" $
    it "checks out a repository" $ withLogging LevelError $ withSystemTempDirectory "base-directory" $ \baseDir -> do
      let repoDir = baseDir </> "repo"
      void $ gitOp $ Git.runGit ["init", toS repoDir]
      writeFile (repoDir </> "hello") "dummy content"
      void $ gitOp $ Git.runGitInRepo repoDir ["add", "hello"]
      void $ gitOp $ Git.runGitInRepo repoDir ["commit", "-m", "Initial commit"]
      let workingTreeDir = baseDir </> "working-tree"
      void $ gitOp $ Git.ensureCheckout repoDir (Git.Branch "master") workingTreeDir
      contents <- readFile (workingTreeDir </> "hello")
      contents `shouldBe` "dummy content"


gitOp :: Monad m => ExceptT Git.GitError m a -> m a
gitOp op = do
  result <- runExceptT op
  case result of
    Left err -> panic $ "git operation failed: " <> show err
    Right res -> pure res
