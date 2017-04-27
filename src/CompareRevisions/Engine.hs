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
import System.FSNotify (Event(..), StopListening, WatchManager, eventPath, watchDir, withManager)

import qualified CompareRevisions.Config as Config
import qualified CompareRevisions.Duration as Duration
import qualified CompareRevisions.Git as Git
import qualified CompareRevisions.Kube as Kube
import CompareRevisions.Kube (KubeObject(..), ImageDiff(..))
import CompareRevisions.Regex (RegexReplace, regexReplace)
import CompareRevisions.Validator (Validator, runValidator, throwE)

-- TODO: Metrics for comparing revisions.

-- | Metrics used in reporting application.
newtype Metrics = Metrics { configFileChanges :: Prom.Metric (Prom.Vector Status Prom.Counter)
                          }

type Status = String
success, failure :: Status
success = "success"
failure = "failure"

-- | Initialize metrics.
initMetrics :: MonadIO io => io Metrics
initMetrics = Metrics
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
  , configFile :: FilePath -- ^ Where the config file lives.
  , config :: TVar ValidConfig  -- ^ Parsed configuration for what images and repositories to compare.
  , diff :: TVar (Maybe ClusterDiff)  -- ^ Result of comparing images and revisions.
  , metrics :: Metrics -- ^ Prometheus metrics for cluster differ
  }

-- | Construct a new 'ClusterDiffer', erroring if the configuration file is invalid.
newClusterDiffer :: (MonadIO m, MonadError Error m) => Config.AppConfig -> m ClusterDiffer
newClusterDiffer Config.AppConfig{..} = do
  config <- loadConfigFile configFile
  configVar <- liftIO (newTVarIO config)
  diff <- liftIO (newTVarIO empty)
  metrics <- initMetrics
  pure $ ClusterDiffer gitRepoDir configFile configVar diff metrics

-- | Get the most recently calculated differences from 'ClusterDiffer'.
getCurrentDifferences :: MonadIO m => ClusterDiffer -> m (Maybe ClusterDiff)
getCurrentDifferences = liftIO . atomically . readTVar . diff

-- | Run a 'ClusterDiffer', looping forever, erroring out if the
-- configuration file is invalid.
runClusterDiffer :: MonadIO m => ClusterDiffer -> ExceptT Error m ()
runClusterDiffer clusterDiffer@ClusterDiffer{..} =
  liftIO $ withManager $ \mgr -> do
    void $ watchFile mgr configFile (configFileChanged clusterDiffer)
    forever loop
  where
    loop = do
      cfg <- atomically . readTVar $ config
      result <- runExceptT $ updateClusterDiff clusterDiffer
      case result of
        Left err -> Log.warn' $ "Updating cluster diff failed: " <> show err
        Right _ -> pass
      Duration.sleep . Config.pollInterval . configRepo $ cfg

-- | Update the current cluster diff between clusters.
updateClusterDiff :: MonadIO io => ClusterDiffer -> ExceptT Error io ()
updateClusterDiff differ@ClusterDiffer{..} = do
  newDiff <- calculateClusterDiff differ
  liftIO . atomically $ writeTVar diff (Just newDiff)

configFileChanged :: (Prom.MonadMonitor m, MonadIO m) => ClusterDiffer -> Event -> m ()
configFileChanged _ (Removed _ _) = pass
configFileChanged _ (Added _ _) = pass
configFileChanged ClusterDiffer{..} (Modified path _) = do
  Log.log' $ "Config file changed: " <> toS path
  result <- runExceptT $ loadConfigFile path
  status <- case result of
    Left err -> do
      Log.warn' $ "Failed to parse config file: " <> show err
      pure failure
    Right cfg -> do
      liftIO $ atomically $ writeTVar config cfg
      pure success
  Prom.withLabel status Prom.incCounter . configFileChanges $ metrics


data LogSpec = LogSpec Git.RevSpec Git.RevSpec (Maybe [FilePath]) deriving (Eq, Ord, Show)

-- | Calculate a new diff between clusters.
calculateClusterDiff :: MonadIO io => ClusterDiffer -> ExceptT Error io ClusterDiff
calculateClusterDiff ClusterDiffer{..} = do
  cfg <- liftIO . atomically . readTVar $ config
  imageDiffs <- compareImages gitRepoDir cfg
  -- XXX: Silently ignoring things that don't have start or end labels, as
  -- well as images that are only deployed on one environment.
  let changedImages = Map.fromList [ (name, (src, tgt)) | Kube.ImageChanged name (Just src) (Just tgt) <- fold imageDiffs ]
  let (withErrors, valid) = Map.mapEitherWithKey (lookupImage cfg) changedImages
  revisionDiffs <- fold <$> mapWithKeyConcurrently compareManyRevs (groupByRepo valid)
  pure (ClusterDiff (revisionDiffs <> map Left withErrors) imageDiffs)
  where
    -- | Given an image name and a source and target label, return the URL for
    -- the Git repository and the 'git log' needed to get the right revisions.
    lookupImage
      :: ValidConfig
      -> Kube.ImageName
      -> (Kube.ImageLabel, Kube.ImageLabel)
      -> Either Error (Git.URL, LogSpec)
    lookupImage cfg name (srcLabel, tgtLabel) = do
      Config.ImageConfig{..} <- note (NoConfigForImage name) (Map.lookup name (images cfg))
      endRev <- labelToRevision imageToRevisionPolicy srcLabel
      startRev <- labelToRevision imageToRevisionPolicy tgtLabel
      pure (gitURL, LogSpec startRev endRev paths)

    -- | Given a map of images to Git repositories and log specs, group the
    -- images names first by repository and then by log spec. This helps us
    -- run expensive git commands as few times as possible.
    groupByRepo
      :: (Ord a, Ord b, Ord c)
      => Map a (b, c)
      -> Map b (Map c [a])
    groupByRepo images =
      Map.unionsWith (Map.unionWith (<>)) [ Map.singleton gitURL (Map.singleton logSpec [imageName])
                                          | (imageName, (gitURL, logSpec)) <- Map.toList images  ]

    -- | Given a variety of log specs, get the logs, and map them to the image
    -- name that needs them.
    compareManyRevs
      :: MonadIO io
      => Git.URL
      -> Map LogSpec [Kube.ImageName]
      -> io (Map Kube.ImageName (Either Error [Git.Revision]))
    compareManyRevs gitURL imagesByLabel = do
      repoPath <- runExceptT $ withExceptT GitError $ syncRepo gitRepoDir gitURL
      case repoPath of
        Left err -> pure $ foldMap (\names -> newMapWithSameValue names (Left err)) imagesByLabel
        Right path -> fold <$> mapWithKeyConcurrently (compareRevs path) imagesByLabel

    -- | Get a single log spec, fanning it out to the image names that need it.
    compareRevs
      :: MonadIO io
      => FilePath
      -> LogSpec
      -> [Kube.ImageName]
      -> io (Map Kube.ImageName (Either Error [Git.Revision]))
    compareRevs repoPath logSpec imageNames = do
      revs <- runExceptT (loadRevs repoPath logSpec)
      pure (newMapWithSameValue imageNames revs)

    loadRevs :: MonadIO io => FilePath -> LogSpec -> ExceptT Error io [Git.Revision]
    loadRevs repoPath (LogSpec startRev endRev paths) = withExceptT GitError $ Git.getLog repoPath startRev endRev paths

    newMapWithSameValue :: Ord key => [key] -> value -> Map key value
    newMapWithSameValue keys value = Map.fromList (zip keys (repeat value))


mapWithKeyConcurrently :: MonadIO io => (k -> a -> IO b) -> Map k a -> io (Map k b)
mapWithKeyConcurrently f d = liftIO $ runConcurrently (Map.traverseWithKey (\k v -> Concurrently (f k v)) d)

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
watchFile :: MonadIO io => WatchManager -> FilePath -> (Event -> IO ()) -> io StopListening
watchFile mgr filePath action = do
  canonicalPath <- liftIO $ canonicalizePath filePath
  let dir = takeDirectory canonicalPath
  liftIO $ watchDir mgr dir ((== canonicalPath) . eventPath) action
