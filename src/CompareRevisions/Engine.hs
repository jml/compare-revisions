{-# LANGUAGE FlexibleContexts #-}
module CompareRevisions.Engine
  ( ValidConfig(..)
  , compareImages
  , loadConfigFile
  ) where


import Protolude

import Control.Concurrent.STM (TVar, readTVar, writeTVar)
import qualified Control.Logging as Log
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map as Map
import Data.String (String)
import Data.Yaml (ParseException, decodeFileEither)
import qualified Prometheus as Prom
import System.Directory (canonicalizePath)
import System.FilePath ((</>), takeDirectory)
import System.FSNotify (Event(..), StopListening, WatchManager, eventPath, watchDir)

import qualified CompareRevisions.Config as Config
import qualified CompareRevisions.Git as Git
import qualified CompareRevisions.Kube as Kube
import CompareRevisions.Kube (KubeObject(..), ImageDiff(..))
import CompareRevisions.Validator (Validator, runValidator, throwE)

-- | Metrics used in reporting application.
newtype Metrics = Metrics { _configFileChanges :: Prom.Metric (Prom.Vector Status Prom.Counter)
                          }

type Status = String
_success, _failure :: Status
_success = "success"
_failure = "failure"

-- | Initialize metrics.
_initMetrics :: MonadIO io => io Metrics
_initMetrics = Metrics
  <$> liftIO (Prom.registerIO (Prom.vector
                                ("status" :: Status)
                                (Prom.counter (Prom.Info
                                                "comparerevisions_config_file_changes_total"
                                                "Number of config file changes detected"))))

data Error
  = ParseError ParseException
  | InvalidConfig (NonEmpty ConfigError)
  deriving (Show)


-- | Need to sync config repo in a loop initially, but should also design system
-- such that there is (can be) a webhook for triggering a sync of the config.
--
-- We could pull all of the Github image repositories at the same time, or we
-- could only pull them when something has changed. Pulling them all the time
-- has the advantage that we can show extra information in the web UI. Pulling
-- them later means less network.
--
-- Once we've updated the config repo, we need to refresh our model. Our model
-- is going to be the 'ImageDiff' generated by the config, possibly mapped
-- from 'ImageName'.
--
-- XXX: Two 'ImageName' types in this repo.
--
-- If the model has changed, then we need to get the Git information
-- associated with that change. Alternatively, we can show that only when
-- someone views the web UI. Probably not, because we want the web UI to be
-- fast.
--
-- Thus, our data model has to include:
--  - Map ImageName (ImageDiff, RevisionLog)
--  - Git SHA-1 of config repository

compareImages
  :: (HasCallStack, MonadError Git.GitError m, MonadIO m)
  => FilePath
  -> Git.URL
  -> Git.Branch
  -> Config.Environment
  -> Config.Environment
  -> m (Map KubeObject [ImageDiff])
compareImages rootDirectory configRepoURL branch src tgt = do
  Git.syncRepo configRepoURL repoPath
  Git.ensureCheckout repoPath branch checkoutPath
  Kube.getDifferingImages <$> loadEnv src <*> loadEnv tgt
  where
    repoPath = rootDirectory </> "repos"
    checkoutPath = rootDirectory </> "config-repo"
    loadEnv env = Kube.loadEnvFromDisk (checkoutPath </> Config.path env)


-- | Configuration we need to compare a cluster.
data ValidConfig
  = ValidConfig
  { configRepo :: Config.ConfigRepo  -- ^ Details of the repository with the Kubernetes manifests.
  , images :: Map Kube.ImageName (Config.ImageConfig Config.PolicyConfig)  -- ^ Information about the source code of images.
  } deriving (Eq, Ord, Show)

-- | Turn a user-specified configuration into a guaranteed valid one.
validateConfig :: Config.Config -> Validator ConfigError ValidConfig
validateConfig (Config.Config repo images policies) =
  ValidConfig repo <$> mappedImages
  where
    mappedImages = Map.traverseWithKey mapImage images
    mapImage :: Kube.ImageName -> Config.ImageConfig Config.PolicyName -> Validator ConfigError (Config.ImageConfig Config.PolicyConfig)
    mapImage imgName img =
      -- XXX: lenses!
      let policyName = Config.imageToRevisionPolicy img
      in case lookupPolicy policyName of
           Nothing -> throwE (UnknownPolicyName imgName policyName)
           Just policy -> pure (img { Config.imageToRevisionPolicy = policy })
    lookupPolicy name = Map.lookup name policies

-- | Errors that can occur in syntactically valid configurations.
data ConfigError
  = UnknownPolicyName Kube.ImageName Config.PolicyName
  deriving (Eq, Ord, Show)

-- | Watch 'AppState' and fire 'configChanged' event if it changes.
--
-- XXX: It's possible this could be folded into 'updateConfig', which only
-- sets the 'TVar' -- why not have it just call 'configChanged'?
_watchConfigs :: MonadIO m => TVar ValidConfig -> (ValidConfig -> m ())  -> m ()
_watchConfigs = watchTVar

-- | Watch a TVar and perform an action when it changes.
watchTVar :: (Eq a, MonadIO m) => TVar a -> (a -> m ())  -> m ()
watchTVar var valueChanged = do
  value <- liftIO . atomically . readTVar $ var
  loop value
  where
    loop val = do
      valueChanged val
      newValue <- liftIO . atomically . blockUntil (/= val) $ var
      loop newValue

    blockUntil p var' = do
      val <- readTVar var'
      if p val
        then pure val
        else retry

_updateConfig :: (Prom.MonadMonitor m, MonadReader Metrics m, MonadIO m) => FilePath -> TVar ValidConfig -> m ()
_updateConfig path configVar = do
  result <- loadConfigFile path
  metrics <- ask
  status <- case result of
    Left err -> do
      Log.warn' $ "Failed to parse config file: " <> show err
      pure _failure
    Right cfg -> do
      liftIO $ atomically $ writeTVar configVar cfg
      pure _success
  Prom.withLabel status Prom.incCounter . _configFileChanges $ metrics

_configFileChanged :: (Prom.MonadMonitor m, MonadReader Metrics m, MonadIO m) => TVar ValidConfig -> Event -> m ()
_configFileChanged _ (Removed _ _) = pure ()
_configFileChanged _ (Added _ _) = pure ()
_configFileChanged configVar (Modified path _) = _updateConfig path configVar

loadConfigFile :: MonadIO io => FilePath -> io (Either Error ValidConfig)
loadConfigFile path = do
  config <- liftIO (decodeFileEither path)
  pure $ case config of
    Left err -> Left (ParseError err)
    Right config' -> first InvalidConfig (runValidator (validateConfig config'))

-- | Watch for changes to a single file, performing 'action' when it happens.
_watchFile :: MonadIO io => WatchManager -> FilePath -> (Event -> IO ()) -> io StopListening
_watchFile mgr filePath action = do
  canonicalPath <- liftIO $ canonicalizePath filePath
  let dir = takeDirectory canonicalPath
  liftIO $ watchDir mgr dir ((== canonicalPath) . eventPath) action
