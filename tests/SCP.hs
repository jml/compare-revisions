module SCP (tests) where

import Protolude
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec, describe, it, shouldBe)

import qualified CompareRevisions.SCP as SCP

tests :: IO TestTree
tests = testSpec "SCP" $ do
  describe "parseSCP" $
    it "parses GitHub remotes" $
      SCP.parseSCP "git@github.com:foo/bar.git" `shouldBe` Just (SCP.AuthRemoteFile "git" "github.com" "foo/bar.git")
  describe "Hostname" $
    it "allows valid hostnames" $
      SCP.makeHostname "github.com" `shouldBe` Just "github.com"
