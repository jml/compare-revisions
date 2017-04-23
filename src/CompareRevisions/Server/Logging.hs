{-# LANGUAGE FlexibleContexts #-}

-- | Logging helpers for multi-git-sync.
module CompareRevisions.Server.Logging
  ( LogLevel(..)
  , flags
  , withLogging
  , error'
  ) where

import Protolude

import Control.Logging
  ( LogLevel(..)
  , flushLog
  , loggingLogger
  , setLogLevel
  , setLogTimeFormat
  , withStdoutLogging
  )
import Options.Applicative
       (Parser, eitherReader, help, long, option, value)


-- | Command-line flags controlling logging.
flags :: Parser LogLevel
flags =
  option
  (eitherReader (pure . fromKeyword . toS))
  (fold
    [ long "log-level"
    , help "Minimum severity for log messages"
    , value LevelInfo
    ])

-- | Log a message at error level, flushing afterward.
error' :: MonadIO io => Text -> io ()
error' message = liftIO $ do
  loggingLogger LevelError message ("" :: Text)
  flushLog

-- | Run an action with logging at the given level.
withLogging :: LogLevel -> IO () -> IO ()
withLogging level action = do
  setLogTimeFormat "%Y-%m-%d %H:%M:%S.%q"
  setLogLevel level
  withStdoutLogging action


type Keyword = Text

fromKeyword :: Keyword -> LogLevel
fromKeyword "error" = LevelError
fromKeyword "warn" = LevelWarn
fromKeyword "info" = LevelInfo
fromKeyword "debug" = LevelDebug
fromKeyword other = LevelOther other

_toKeyword :: LogLevel -> Keyword
_toKeyword LevelError = "error"
_toKeyword LevelWarn = "warn"
_toKeyword LevelInfo = "info"
_toKeyword LevelDebug = "debug"
_toKeyword (LevelOther other) = other
