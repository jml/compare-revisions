{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
module CompareRevisions.Engine
  ( Error(..)
  , ClusterDiffer
  , newClusterDiffer
  , runClusterDiffer
  , ClusterDiff(..)
  , getCurrentDifferences
  ) where

import Protolude

import Control.Concurrent.STM (TVar, newTVarIO, readTVar, writeTVar)
import qualified Control.Logging as Log
import Control.Monad.Except (withExceptT)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map as Map
import Data.String (String)
import Data.Yaml (ParseException, decodeFileEither)
import qualified Prometheus as Prom
import System.Directory (canonicalizePath)
import System.FilePath ((</>), takeDirectory)
import System.FSNotify (Event(..), StopListening, WatchManager, eventPath, watchDir)

import qualified CompareRevisions.Config as Config
import qualified CompareRevisions.Duration as Duration
import qualified CompareRevisions.Git as Git
import qualified CompareRevisions.Kube as Kube
import CompareRevisions.Kube (KubeObject(..), ImageDiff(..))
import CompareRevisions.Regex (RegexReplace, regexReplace)
import CompareRevisions.Validator (Validator, runValidator, throwE)

-- TODO: Actually watch config file for changes, then update.

-- TODO: Metrics for comparing revisions.

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
  | RegexError RegexReplace Text
  | GitError Git.GitError
  | NoConfigForImage Kube.ImageName
  deriving (Show)


-- | The differences between two clusters at a point in time.
data ClusterDiff
 = ClusterDiff
 { revisionDiffs :: Map Kube.ImageName (Either Error [Git.Revision])
 , imageDiffs :: Map KubeObject [ImageDiff]
 }
 deriving (Show)

-- | Core application for comparing revisions of images.
data ClusterDiffer
  = ClusterDiffer
  { gitRepoDir :: FilePath  -- ^ Path to where we'll put the Git repositories.
  , config :: ValidConfig  -- ^ Parsed configuration for what images and repositories to compare.
  , diff :: TVar (Maybe ClusterDiff)  -- ^ Result of comparing images and revisions.
  }

-- | Construct a new 'ClusterDiffer', erroring if the configuration file is invalid.
newClusterDiffer :: (MonadIO m, MonadError Error m) => Config.AppConfig -> m ClusterDiffer
newClusterDiffer Config.AppConfig{..} = do
  config <- loadConfigFile configFile
  ClusterDiffer gitRepoDir config <$> (liftIO . newTVarIO) empty

-- | Get the most recently calculated differences from 'ClusterDiffer'.
getCurrentDifferences :: MonadIO m => ClusterDiffer -> m (Maybe ClusterDiff)
getCurrentDifferences = liftIO . atomically . readTVar . diff

-- | Run a 'ClusterDiffer', looping forever, erroring out if the
-- configuration file is invalid.
runClusterDiffer :: MonadIO m => ClusterDiffer -> ExceptT Error m ()
runClusterDiffer clusterDiffer@ClusterDiffer{..} = do
  updateClusterDiff clusterDiffer
  liftIO . Duration.sleep . Config.pollInterval . configRepo $ config
  runClusterDiffer clusterDiffer

-- | Update the current cluster diff between clusters.
updateClusterDiff :: MonadIO io => ClusterDiffer -> ExceptT Error io ()
updateClusterDiff differ@ClusterDiffer{..} = do
  newDiff <- calculateClusterDiff differ
  liftIO . atomically $ writeTVar diff (Just newDiff)

-- | Calculate a new diff between clusters.
calculateClusterDiff :: MonadIO io => ClusterDiffer -> ExceptT Error io ClusterDiff
calculateClusterDiff ClusterDiffer{..} = do
  imageDiffs <- compareImages gitRepoDir config
  -- XXX: Silently ignoring things that don't have start or end labels, as
  -- well as images that are only deployed on one environment.
  let changedImages = Map.fromList [ (name, (src, tgt)) | Kube.ImageChanged name (Just src) (Just tgt) <- foldMap identity imageDiffs ]
  revisionDiffs <- Map.traverseWithKey getRevisions changedImages
  pure (ClusterDiff revisionDiffs imageDiffs)
  where
    getRevisions :: MonadIO m => Kube.ImageName -> (Kube.ImageLabel, Kube.ImageLabel) -> m (Either Error [Git.Revision])
    getRevisions name (start, end) = runExceptT $ do
      Config.ImageConfig{..} <- note (NoConfigForImage name) (Map.lookup name (images config))
      compareRevisions gitRepoDir imageToRevisionPolicy gitURL paths start end

-- | Get the list of revisions that were added between two versions of an image.
compareRevisions
  :: (MonadIO m)
  => FilePath -- ^ Root directory for compare-revisions. Where we store downloaded repositories.
  -> Config.PolicyConfig -- ^ How to interpret image labels
  -> Git.URL -- ^ Where the code for the image can be found
  -> Maybe [FilePath]  -- ^ Optional paths to filter logs by
  -> Kube.ImageLabel -- ^ The image label in the source environment
  -> Kube.ImageLabel -- ^ The image label in the target environment
  -> ExceptT Error m [Git.Revision]
compareRevisions rootDirectory labelPolicy repoURL paths srcLabel tgtLabel = do
  -- XXX: This duplicates work. Multiple images have the same Git repo, so we
  -- only need to sync it once.
  repoPath <- withExceptT GitError $ syncRepo rootDirectory repoURL
  -- TODO: Express this applicatively.
  endRev <- labelToRevision labelPolicy srcLabel
  startRev <- labelToRevision labelPolicy tgtLabel
  withExceptT GitError $ Git.getLog repoPath startRev endRev paths

-- | Sync repository underneath our root directory, returning the path of the
-- repository locally.
syncRepo :: (MonadIO m, MonadError Git.GitError m) => FilePath -> Git.URL -> m FilePath
syncRepo repoRoot url = do
  Git.syncRepo url repoPath
  pure repoPath
  where
    repoPath = Config.getRepoPath repoRoot url

compareImages :: MonadIO io => FilePath -> ValidConfig -> ExceptT Error io (Map KubeObject [ImageDiff])
compareImages gitRepoDir ValidConfig{..} = withExceptT GitError $ do
  let Config.ConfigRepo{..} = configRepo
  repoPath <- syncRepo gitRepoDir url
  Git.ensureCheckout repoPath (fromMaybe (Git.Branch "master") branch) checkoutPath
  Kube.getDifferingImages <$> loadEnv sourceEnv <*> loadEnv targetEnv
  where
    checkoutPath = gitRepoDir </> "config-repo"
    loadEnv env = Kube.loadEnvFromDisk (checkoutPath </> Config.path (env :: Config.Environment))

-- | Get the Git revision corresponding to a particular label. Error if we
-- can't figure it out.
labelToRevision :: MonadError Error m => Config.PolicyConfig -> Kube.ImageLabel -> m Git.RevSpec
labelToRevision Config.Identity label = pure . Git.RevSpec $ label
labelToRevision (Config.Regex regex) label = Git.RevSpec . toS <$> note (RegexError regex label) (regexReplace regex (toS label))


-- | Information on all the images.
type ImagePolicies = Map Kube.ImageName (Config.ImageConfig Config.PolicyConfig)

-- | Configuration we need to compare a cluster.
data ValidConfig
  = ValidConfig
  { configRepo :: Config.ConfigRepo  -- ^ Details of the repository with the Kubernetes manifests.
  , images :: ImagePolicies  -- ^ Information about the source code of images.
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
  result <- runExceptT $ loadConfigFile path
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

loadConfigFile :: (MonadError Error io, MonadIO io) => FilePath -> io ValidConfig
loadConfigFile path = do
  config <- liftIO (decodeFileEither path)
  case config of
    Left err -> throwError (ParseError err)
    Right config' ->
      case runValidator (validateConfig config') of
        Left err -> throwError (InvalidConfig err)
        Right result -> pure result

-- | Watch for changes to a single file, performing 'action' when it happens.
_watchFile :: MonadIO io => WatchManager -> FilePath -> (Event -> IO ()) -> io StopListening
_watchFile mgr filePath action = do
  canonicalPath <- liftIO $ canonicalizePath filePath
  let dir = takeDirectory canonicalPath
  liftIO $ watchDir mgr dir ((== canonicalPath) . eventPath) action
