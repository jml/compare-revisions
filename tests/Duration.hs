module Duration (tests) where

import Protolude
import Test.Hspec.QuickCheck (prop)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec, describe, it, shouldBe)

import qualified CompareRevisions.Duration as Duration

tests :: IO TestTree
tests = testSpec "Duration" $ do
  describe "formatDuration and parseDuration" $
    prop "roundtrip" $
      \x -> Duration.parseDuration (Duration.formatDuration x) == Just x
  describe "formatDuration formats" $ do
    it "0" $
      Duration.formatDuration 0 `shouldBe` "0s"
    it "whole seconds" $
      Duration.formatDuration ((4 :: Double) `Duration.mul` Duration.second) `shouldBe` "4s"
    it "microseconds" $
      Duration.formatDuration 1000 `shouldBe` "1us"
    it "fractional microseconds" $
      Duration.formatDuration 1003 `shouldBe` "1.003us"
    it "minutes" $
      Duration.formatDuration 60000000000 `shouldBe` "1m0s"
  describe "parseDuration parses" $ do
    it "0s" $
      Duration.parseDuration "0s" `shouldBe` Just 0
    it "1us" $
      Duration.parseDuration "1us" `shouldBe` Just 1000
    it "1.003us" $
      Duration.parseDuration "1.003us" `shouldBe` Just 1003
    it "1m0s" $ do
      Duration.parseDuration "1m" `shouldBe` Just 60000000000
      Duration.parseDuration "1m0s" `shouldBe` Just 60000000000
  describe "mul" $
    it "allows fractional multipliers" $
      (1.003 :: Double) `Duration.mul` Duration.microsecond `shouldBe` 1003
