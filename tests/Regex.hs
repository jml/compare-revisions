module Regex (tests) where

import Protolude

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec, describe, it, shouldBe)

import qualified CompareRevisions.Regex as Regex

tests :: IO TestTree
tests = testSpec "Regex" $
  describe "compileReplacement" $ do
    it "does very little on strings without escapes" $
      Regex.compileReplacement "hello" `shouldBe` [Right "hello"]
    it "inverts" $
      -- NOTE: This was a property test, but it turns out that I don't know
      -- what we should do when the escaped number is preceded by zeroes.
      rebuild (Regex.compileReplacement "foo\\1bar") `shouldBe` "foo\\1bar"

  where
    rebuild = mconcat . map (either escaped identity)
    escaped :: Int -> ByteString
    escaped n = "\\" <> show n
