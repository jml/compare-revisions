{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
module CompareRevisions.Engine
  ( Error(..)
  , ClusterDiffer
  , newClusterDiffer
  , runClusterDiffer
  , ClusterDiff(..)
  , getCurrentDifferences
  ) where

import Protolude hiding (diff, throwE)

import Control.Concurrent.STM (TVar, newTVarIO, readTVar, writeTVar)
import qualified Control.Logging as Log
import Control.Monad.Except (withExceptT)
import qualified Data.Map as Map
import Data.String (String)
import qualified Prometheus as Prom
import System.Directory (canonicalizePath)
import System.FilePath ((</>), takeDirectory)
import System.FSNotify (Event(..), StopListening, WatchManager, eventPath, watchDir, withManager)

import qualified CompareRevisions.Config as Config
import qualified CompareRevisions.Duration as Duration
import qualified CompareRevisions.Git as Git
import qualified CompareRevisions.Kube as Kube
import CompareRevisions.Regex (RegexReplace, regexReplace)

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
  = InvalidConfig Config.Error
  | RegexError RegexReplace Text
  | GitError Git.GitError
  | NoConfigForImage Kube.ImageName
  deriving (Show)


-- | The differences between two clusters at a point in time.
data ClusterDiff
 = ClusterDiff
 { revisionDiffs :: Map Kube.ImageName (Either Error [Git.Revision])
 , imageDiffs :: Map Kube.KubeObject [Kube.ImageDiff]
 }
 deriving (Show)

-- | Core application for comparing revisions of images.
data ClusterDiffer
  = ClusterDiffer
  { gitRepoDir :: FilePath  -- ^ Path to where we'll put the Git repositories.
  , configFile :: FilePath -- ^ Where the config file lives.
  , config :: TVar Config.ValidConfig  -- ^ Parsed configuration for what images and repositories to compare.
  , diff :: TVar (Maybe ClusterDiff)  -- ^ Result of comparing images and revisions.
  , metrics :: Metrics -- ^ Prometheus metrics for cluster differ
  }

-- | Construct a new 'ClusterDiffer', erroring if the configuration file is invalid.
newClusterDiffer :: Config.AppConfig -> ExceptT Error IO ClusterDiffer
newClusterDiffer Config.AppConfig{..} = do
  possiblyConfig <- runExceptT $ Config.loadConfigFile configFile
  config <- case possiblyConfig of
              Left err -> throwError (InvalidConfig err)
              Right cfg -> pure cfg
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
      Duration.sleep . Config.pollInterval . Config.configRepo $ cfg

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
  result <- runExceptT $ Config.loadConfigFile path
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
calculateClusterDiff ClusterDiffer{gitRepoDir, config} = do
  cfg <- liftIO . atomically . readTVar $ config
  imageDiffs <- compareImages gitRepoDir cfg
  revisionDiffs <- compareRevisions gitRepoDir (Config.images cfg) (fold imageDiffs)
  pure (ClusterDiff revisionDiffs imageDiffs)


-- | Find the Git revisions that lie between many different versions of images.
--
-- We compare many image differences at once, because multiple images are
-- often backed by the same Git repository. Comparing many at once allows us
-- to sync each Git repository only once, which reduces the work we have to
-- do.
compareRevisions
  :: MonadIO m
  => FilePath  -- ^ Path on disk to where all the Git repositories are.
  -> Map Kube.ImageName (Config.ImageConfig Config.PolicyConfig)  -- ^ How we go from the image name to Git.
  -> [Kube.ImageDiff]  -- ^ A set of differences between images.
  -> m (Map Kube.ImageName (Either Error [Git.Revision]))  -- ^ For each image, either the Git revisions that have changed or an error.
compareRevisions gitRepoDir imagePolicies imageDiffs  = do
  -- XXX: Silently ignoring things that don't have start or end labels, as
  -- well as images that are only deployed on one environment.
  let changedImages = Map.fromList [ (name, (src, tgt)) | Kube.ImageChanged name (Just src) (Just tgt) <- imageDiffs ]
  -- For each image, find out the Git repository we need to fetch and the log command we need to run.
  -- If we can't find this information, collect the image & the error message into withErrors.
  let (withErrors, valid) = Map.mapEitherWithKey lookupImage changedImages
  -- Sync the repos and fetch logs.
  revisionDiffs <- fold <$> mapWithKeyConcurrently compareManyRevs (groupByRepo valid)
  pure (revisionDiffs <> map Left withErrors)
  where
    -- | Given an image name and a source and target label, return the URL for
    -- the Git repository and the 'git log' needed to get the right revisions.
    lookupImage
      :: Kube.ImageName
      -> (Kube.ImageLabel, Kube.ImageLabel)
      -> Either Error (Git.URL, LogSpec)
    lookupImage name (srcLabel, tgtLabel) = do
      imageConfig <- note (NoConfigForImage name) (Map.lookup name imagePolicies)
      endRev <- labelToRevision (Config.imageToRevisionPolicy imageConfig) srcLabel
      startRev <- labelToRevision (Config.imageToRevisionPolicy imageConfig) tgtLabel
      pure (Config.gitURL imageConfig, LogSpec startRev endRev (Config.paths imageConfig))

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
    --
    -- The type signature is a little backwards so we can avoid fetching the same log spec many times over.
    compareManyRevs
      :: MonadIO io
      => Git.URL  -- ^ The URL of the Git repository we want to inspect.
      -> Map LogSpec [Kube.ImageName]  -- ^ The revision logs we want, associated with the (possibly many) images that changed in the way that corresponded to these logs.
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


-- | Run a function on every key in a map at the same time.
mapWithKeyConcurrently
  :: MonadIO io
  => (k -> a -> IO b) -- ^ The function to run concurrently. 'k' is the key, and 'a' is the value of that key.
  -> Map k a  -- ^ The map to run the function on.
  -> io (Map k b)  -- ^ A new map with the same keys as the first, and values derived from the given function.
mapWithKeyConcurrently f d = liftIO $ runConcurrently (Map.traverseWithKey (\k v -> Concurrently (f k v)) d)

-- | Sync repository underneath our root directory, returning the path of the
-- repository locally.
syncRepo :: (MonadIO m, MonadError Git.GitError m) => FilePath -> Git.URL -> m FilePath
syncRepo repoRoot url = do
  Git.syncRepo url repoPath
  pure repoPath
  where
    repoPath = Config.getRepoPath repoRoot url

compareImages :: MonadIO io => FilePath -> Config.ValidConfig -> ExceptT Error io (Map Kube.KubeObject [Kube.ImageDiff])
compareImages gitRepoDir Config.ValidConfig{..} = withExceptT GitError $ do
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


-- | Watch for changes to a single file, performing 'action' when it happens.
watchFile :: MonadIO io => WatchManager -> FilePath -> (Event -> IO ()) -> io StopListening
watchFile mgr filePath action = do
  canonicalPath <- liftIO $ canonicalizePath filePath
  let dir = takeDirectory canonicalPath
  liftIO $ watchDir mgr dir ((== canonicalPath) . eventPath) action
