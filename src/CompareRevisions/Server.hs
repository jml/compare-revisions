-- | Generic web server stuff.
module CompareRevisions.Server
  ( Config
  , flags
  , run
  ) where

import Protolude

import Control.Logging (LogLevel(..), log', warn', setLogLevel, setLogTimeFormat, withStdoutLogging)
import GHC.Stats (getGCStatsEnabled)
import Network.Wai.Handler.Warp
       (Port, Settings, defaultSettings, runSettings, setBeforeMainLoop,
        setPort)
import qualified Network.Wai.Middleware.RequestLogger as RL
import Options.Applicative
       (Parser, auto, eitherReader, help, long, metavar, option, switch, value)
import qualified Prometheus as Prom
import qualified Prometheus.Metric.GHC as Prom
import Servant (Application)

import CompareRevisions.Server.Instrument
       (defaultPrometheusSettings, prometheus, requestDuration)
import qualified CompareRevisions.Server.Logging as Log


-- | Generic web server configuration.
data Config = Config
  { port :: Port  -- ^ Port to listen on
  , accessLogs :: AccessLogs  -- ^ Level of access logs to display
  , logLevel :: LogLevel  -- ^ Level of regular logs to display
  , enableGhcMetrics :: Bool  -- ^ Whether to include Prometheus metrics for GHC runtime stats
  } deriving (Eq, Show)

-- | Command-line flags for generating 'Config'.
flags :: Parser Config
flags =
  Config
  <$> option auto (fold [long "port", metavar "PORT", help "Port to listen on"])
  <*> option
        (eitherReader parseAccessLogs)
        (fold
           [long "access-logs", help "How to log HTTP access", value Disabled])
  <*> option
        (eitherReader (pure . Log.fromKeyword . toS))
        (fold
           [ long "log-level"
           , help "Minimum severity for log messages"
           , value LevelInfo
           ])
  <*> switch
        (fold
           [ long "ghc-metrics"
           , help "Export GHC metrics. Requires running with +RTS."
           ])
  where
    parseAccessLogs "none" = pure Disabled
    parseAccessLogs "basic" = pure Enabled
    parseAccessLogs "dev" = pure DevMode
    parseAccessLogs _ = throwError "One of 'none', 'basic', or 'dev'"


-- | What level of access logs to show.
data AccessLogs
  = Disabled -- ^ Don't show access logs.
  | Enabled -- ^ Show Apache-style access logs.
  | DevMode -- ^ Show detailed, colorful access logs. Not suitable in production.
  deriving (Eq, Show)

-- | Run a web server for 'app'. Blocks until the server is shut down.
run :: Config -> Application -> IO ()
run config@Config{..} app = do
  setLogTimeFormat "%Y-%m-%d %H:%M:%S.%q"
  setLogLevel logLevel
  withStdoutLogging $ do
    requests <- Prom.registerIO requestDuration
    when enableGhcMetrics $
      do statsEnabled <- getGCStatsEnabled
         unless statsEnabled $
           warn' "Exporting GHC metrics but GC stats not enabled. Re-run with +RTS -T."
         void $ Prom.register Prom.ghcMetrics
    runSettings settings (middleware requests)
  where
    settings = warpSettings config
    middleware r =
      logging . prometheus defaultPrometheusSettings r "compare_revisions" $ app
    logging =
      case accessLogs of
        Disabled -> identity
        Enabled -> RL.logStdout
        DevMode -> RL.logStdoutDev

-- | Generate warp settings from config
--
-- Serve from a port and print out where we're serving from.
warpSettings :: Config -> Settings
warpSettings Config{..} =
  setBeforeMainLoop printPort (setPort port defaultSettings)
  where
    printPort = log' ("Listening on :" <> show port)
