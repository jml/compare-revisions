-- | Launch compare-revisions server.
module Main
  ( main
  ) where

import Protolude

import CompareRevisions.Server (startApp)

main :: IO ()
main = startApp
