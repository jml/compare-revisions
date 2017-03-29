module Main
  ( main
  ) where

import Protolude

import Test.Tasty (defaultMain, testGroup)

import qualified Config
import qualified Duration
import qualified SCP
import qualified Server

main :: IO ()
main = do
  t <- sequence tests
  defaultMain . testGroup "CompareRevisions" $ t
  where
    tests =
      [ Server.tests
      , SCP.tests
      , Duration.tests
      , Config.tests
      ]
