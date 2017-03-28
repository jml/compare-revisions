module Main
  ( main
  ) where

import Protolude

import Test.Tasty (defaultMain, testGroup)

import qualified Duration
import qualified Server

main :: IO ()
main = do
  t <- sequence tests
  defaultMain . testGroup "CompareRevisions" $ t
  where
    tests =
      [ Server.tests
      , Duration.tests
      ]
