module Regex (tests) where

import Protolude

import Data.String (String)
import Test.Hspec.QuickCheck (prop)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec, describe, it, shouldBe)

import qualified CompareRevisions.Regex as Regex

tests :: IO TestTree
tests = testSpec "Regex" $
  describe "compileReplacement" $ do
    it "does very little on strings without escapes" $
      Regex.compileReplacement "hello" `shouldBe` [Right "hello"]
    prop "is invertable" $
      \x -> let byteString = toS (x :: String) in rebuild (Regex.compileReplacement byteString) == byteString

  where
    rebuild = mconcat . map (either escaped identity)
    escaped :: Int -> ByteString
    escaped n = "\\" <> show n
