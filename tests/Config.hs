{-# LANGUAGE OverloadedLists #-}
module Config (tests) where

import Protolude
import qualified Data.Text as Text
import qualified Data.Yaml as Yaml
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec, describe, it, shouldBe)

import qualified CompareRevisions.Config as Config
import qualified CompareRevisions.Duration as Duration
import qualified CompareRevisions.Git as Git
import CompareRevisions.SCP (SCP(..))

tests :: IO TestTree
tests = testSpec "Config" $ do
  describe "Parser" $
    it "Parses the example in the README" $
      Yaml.decodeEither (toS readmeExample) `shouldBe` Right parsedReadmeExample

  describe "Revision policies" $
    it "Parses the example in the README" $ do
      let example = Text.unlines
            [ "type: regex"
            , "match: ^master-([0-9a-f]+)$"
            , "output: \\1"
            ]
      let expected = Config.Regex { match = "^master-([0-9a-f]+)$", output = "\\1" }
      Yaml.decodeEither (toS example) `shouldBe` Right expected

readmeExample :: Text
readmeExample = Text.unlines
  [ "config-repo:"
  , "  url: git@github.com:my-org/service-config.git"
  , "  poll-interval: 1m"
  , "  source-env:"
  , "    name: dev"
  , "    path: k8s/dev"
  , "  target-env:"
  , "    name: prod"
  , "    path: k8s/prod"
  , ""
  , "images:"
  , "  weaveworks/cortex:"
  , "    git-url: git@github.com:weaveworks/cortex.git"
  , "    image-to-revision-policy: weaveworks"
  , ""
  , "revision-policies:"
  , "  weaveworks:"
  , "    type: regex"
  , "    match: ^master-([0-9a-f]+)$"
  , "    output: \\1"
  ]

parsedReadmeExample :: Config.Config
parsedReadmeExample =
  Config.Config repo images policies
  where
    repo = Config.ConfigRepo
           { url = Git.SCP (AuthRemoteFile "git" "github.com" "my-org/service-config.git")
           , pollInterval = 1 * Duration.minute
           , sourceEnv = Config.Environment
                         { name = "dev"
                         , path = "k8s/dev"
                         }
           , targetEnv = Config.Environment
                         { name = "prod"
                         , path = "k8s/prod"
                         }
           }
    images = [ ("weaveworks/cortex", Config.ImageConfig
                                     { gitURL = Git.SCP (AuthRemoteFile "git" "github.com" "weaveworks/cortex.git")
                                     , imageToRevisionPolicy = "weaveworks"
                                     })
             ]
    policies = [ ("weaveworks", Config.Regex
                                { match = "^master-([0-9a-f]+)$"
                                , output = "\\1"
                                })
               ]
