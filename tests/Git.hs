{-# LANGUAGE FlexibleContexts #-}
module Git (tests) where

import Protolude

import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Time as Time
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec, describe, it, shouldBe)

import qualified CompareRevisions.Git as Git
import CompareRevisions.Server.Logging (withLogging, LogLevel(..))

tests :: IO TestTree
tests = testSpec "Git" $ do
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
  describe "log parsing" $
    it "parses correct logs" $ do
      let example = ByteString.unlines
            [ "commit 5ec96380b9cede27c29f62a547a21c0e50c0e615"
            , "Author:     Jonathan Lange <jml@mumak.net>"
            , "AuthorDate: 2018-03-29 09:06:32 +0100"
            , "Commit:     Jonathan Lange <jml@mumak.net>"
            , "CommitDate: 2018-03-29 09:06:32 +0100"
            , ""
            , "    Show revision in output"
            , ""
            , "commit 2c141409774c95bb0f8e6db9773c50de8c4dc6ea"
            , "Merge: 413dd27 06b2a5d"
            , "Author:     bryan <bryan@weave.works>"
            , "AuthorDate: 2018-03-27 13:08:24 +0000"
            , "Commit:     Weave Flux <support@weave.works>"
            , "CommitDate: 2018-03-27 13:08:24 +0000"
            , ""
            , "    I want to leave a bit longer for cortex#681 to be tested in dev"
            , "    "
            , "    - Locked: cortex:deployment/distributor"
            , "    - Updated policies: cortex:deployment/distributor"
            , ""
            , "commit 66474fb8229ba7bcf52c228fda91fb89b0920fe7"
            , "Author:     Jonathan Lange <jml@mumak.net>"
            , "AuthorDate: 2018-03-29 08:35:51 +0100"
            , "Commit:     Jonathan Lange <jml@mumak.net>"
            , "CommitDate: 2018-03-29 08:35:51 +0100"
            , ""
            , "    List revisions in reverse chronological order"
            ]
      let parsed = Git.parseFullerRevisions example
      parsed `shouldBe`
        Right [ Git.Revision
                { revisionHash = Git.Hash "5ec96380b9cede27c29f62a547a21c0e50c0e615"
                , commitDate = Time.UTCTime
                               { utctDay = Time.fromGregorian 2018 3 29
                               , utctDayTime = Time.timeOfDayToTime $ Time.TimeOfDay 8 6 32
                               }
                , authorName = "Jonathan Lange"
                , subject = "Show revision in output"
                , body = Nothing
                }
              , Git.Revision
                { revisionHash = Git.Hash "2c141409774c95bb0f8e6db9773c50de8c4dc6ea"
                , commitDate = Time.UTCTime
                               { utctDay = Time.fromGregorian 2018 3 27
                               , utctDayTime = Time.timeOfDayToTime $ Time.TimeOfDay 13 8 24
                               }
                , authorName = "bryan"
                , subject = "I want to leave a bit longer for cortex#681 to be tested in dev"
                , body = Just "- Locked: cortex:deployment/distributor\n- Updated policies: cortex:deployment/distributor\n"
                }
              , Git.Revision
                { revisionHash = Git.Hash "66474fb8229ba7bcf52c228fda91fb89b0920fe7"
                , commitDate = Time.UTCTime
                               { utctDay = Time.fromGregorian 2018 3 29
                               , utctDayTime = Time.timeOfDayToTime $ Time.TimeOfDay 7 35 51
                               }
                , authorName = "Jonathan Lange"
                , subject = "List revisions in reverse chronological order"
                , body = Nothing
                }
              ]

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
