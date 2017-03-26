-- | Launch compare-revisions server.
module Main
  ( main
  ) where

import Protolude

import CompareRevisions (startApp)

main :: IO ()
main = startApp
