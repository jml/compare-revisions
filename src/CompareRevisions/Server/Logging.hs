{-# LANGUAGE FlexibleContexts #-}

-- | Logging helpers for multi-git-sync.
module CompareRevisions.Server.Logging
  ( fromKeyword
  , toKeyword
  ) where

import Protolude

import Control.Logging (LogLevel(..))


type Keyword = Text

fromKeyword :: Keyword -> LogLevel
fromKeyword "error" = LevelError
fromKeyword "warn" = LevelWarn
fromKeyword "info" = LevelInfo
fromKeyword "debug" = LevelDebug
fromKeyword other = LevelOther other

toKeyword :: LogLevel -> Keyword
toKeyword LevelError = "error"
toKeyword LevelWarn = "warn"
toKeyword LevelInfo = "info"
toKeyword LevelDebug = "debug"
toKeyword (LevelOther other) = other
