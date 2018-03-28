{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
-- | The heart of compare-revisions
--
-- The application's main core is 'ClusterDiffer', which is responsible for
-- repeatedly updating the 'ClusterDiff'--a static representation of the
-- differences between two clusters.
module CompareRevisions.Engine
  ( Error(..)
  , ClusterDiffer
  , newClusterDiffer
  , runClusterDiffer
  , getConfig
  , ClusterDiff(..)
  , getCurrentDifferences
  ) where

import Protolude hiding (diff, throwE)

import Control.Concurrent.Async (mapConcurrently)
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

-- | An error that occurs while comparing repositories
data Error
  = -- | The configuration file was broken somehow.
    InvalidConfig Config.Error
    -- | One of the regexes in the config file could not be applied.
  | RegexError RegexReplace Text
    -- | We could not fetch Git repositories or calculate Git logs.
  | GitError Git.GitError
    -- | We could not figure out Git information for the given image.
  | NoConfigForImage Kube.ImageName
  deriving (Show)


-- | The differences between two clusters at a point in time.
data ClusterDiff
 = ClusterDiff
 { revisionDiffs :: Map Kube.ImageName (Either Error [Git.Revision])
 , imageDiffs :: Map Kube.KubeID [Kube.ImageDiff]
 }
 deriving (Show)

-- | Core application for comparing revisions of images.
--
-- Once run (with 'runClusterDiffer'), it will run in a loop, forever updating
-- the cluster diff ('updateClusterDiff'). The current diff can be got with
-- 'getCurrentDifferences'.
--
-- Construct with 'newClusterDiffer'.
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

-- | Get the configuration of the differ.
getConfig :: MonadIO io => ClusterDiffer -> io Config.ValidConfig
getConfig ClusterDiffer{config} = liftIO . atomically . readTVar $ config

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

-- | The configuration file has changed.
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


-- | A log command to run. Has the start revision, end revision, and an optional set of paths to restrict the log to.
data LogSpec = LogSpec Git.RevSpec Git.RevSpec (Maybe [FilePath]) deriving (Eq, Ord, Show)

-- | Calculate a new diff between clusters.
calculateClusterDiff :: MonadIO io => ClusterDiffer -> ExceptT Error io ClusterDiff
calculateClusterDiff differ@ClusterDiffer{gitRepoDir} = do
  cfg <- getConfig differ
  let Config.ConfigRepo{url, branch, sourceEnv, targetEnv} = Config.configRepo cfg
  imageDiffs <- compareImages gitRepoDir url branch (Config.path sourceEnv) (Config.path targetEnv)
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
  let (withErrors, comparableImages) = Map.mapEitherWithKey lookupImage changedImages
  let repoToLogSpecs = Map.fromListWith (<>) [(gitURL, [logSpec]) | (gitURL, logSpec) <- Map.elems comparableImages ]
  -- Sync the repos and fetch logs.
  repoToLogs <- fetchGitLogs gitRepoDir repoToLogSpecs
  -- Re-associate the fetched logs with image names.
  let reversedIndex = Map.fromListWith (<>) [(v, [k]) | (k, v) <- Map.toList comparableImages]
  let imageToLogs = flip Map.foldMapWithKey repoToLogs $ \repo logSpecsToLogs ->
        flip Map.foldMapWithKey logSpecsToLogs $ \logSpec revisions ->
        case Map.lookup (repo, logSpec) reversedIndex of
          Nothing ->
            -- XXX: Technically, this should never happen.
            -- Other options include panicking here (to catch the bug early),
            -- or restructuring the code to avoid the bug entirely,
            -- by passing the images through to `fetchGitLogs` and including them in the result.
            Map.empty
          Just indexes -> Map.fromList (zip indexes (repeat revisions))
  -- Include all the images we couldn't compare due to config defects.
  pure (imageToLogs <> map Left withErrors)
  where
    -- | Given an image name and a source and target label, return the URL for
    -- the Git repository and the 'git log' needed to get the right revisions.
    lookupImage
      :: Kube.ImageName
      -> (Kube.ImageLabel, Kube.ImageLabel)
      -> Either Error (Git.URL, LogSpec)
    lookupImage name (srcLabel, tgtLabel) = do
      imageConfig <- note (NoConfigForImage name) (Map.lookup name imagePolicies)
      endRev <- labelToRevision imageConfig srcLabel
      startRev <- labelToRevision imageConfig tgtLabel
      pure (Config.gitURL imageConfig, LogSpec startRev endRev (Config.paths imageConfig))


-- | Fetch many Git logs for many repositories.
--
-- Fetches the repositories locally and then runs the log commands.
fetchGitLogs
  :: MonadIO io
  => FilePath  -- ^ Directory that contains all the git repositories
  -> Map Git.URL [LogSpec] -- ^ Map from Git repositories to fetch to the log commands we want to run against each repo
  -> io (Map Git.URL (Map LogSpec (Either Error [Git.Revision])))  -- ^ For each repo, for each log command, the result of running that command
fetchGitLogs gitRepoDir = mapWithKeyConcurrently compareManyRevs
  where
    -- | Given a variety of log specs, get the logs, and map them to the image
    -- name that needs them.
    --
    -- The type signature is a little backwards so we can avoid fetching the same log spec many times over.
    compareManyRevs
      :: Git.URL  -- ^ The URL of the Git repository we want to inspect.
      -> [LogSpec]  -- ^ The revision logs we want to run against this repository.
      -> IO (Map LogSpec (Either Error [Git.Revision]))
    compareManyRevs gitURL logSpecs = do
        repoPath <- runExceptT $ withExceptT GitError $ syncRepo gitRepoDir gitURL
        results <- case repoPath of
          Left err -> pure $ repeat (Left err)
          Right path -> mapConcurrently (loadRevs path) logSpecs
        pure $ Map.fromList (zip logSpecs results)

    -- | Get a single log spec
    loadRevs :: MonadIO io => FilePath -> LogSpec -> io (Either Error [Git.Revision])
    loadRevs repoPath (LogSpec startRev endRev paths) = runExceptT $ withExceptT GitError $ Git.getLog repoPath startRev endRev paths


-- | Run a function on every key in a map at the same time.
-- Result is a new map with the same keys but different values.
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

-- | Find the images that differ between two Kubernetes environments.
compareImages
  :: MonadIO io
  => FilePath  -- ^ Where all of the Git repositories are
  -> Git.URL  -- ^ The URL of the repository with the Kubernetes objects (aka the config repo)
  -> Maybe Git.Branch  -- ^ The branch of the repository with the configuration. If Nothing, assume "master".
  -> FilePath  -- ^ The path to the source environment (e.g. "k8s/dev")
  -> FilePath  -- ^ The path to the target environment (e.g. "k8s/prod")
  -> ExceptT Error io (Map Kube.KubeID [Kube.ImageDiff])  -- ^ A map of Kubernetes objects to lists of differences between images.
compareImages gitRepoDir url branch sourceEnv targetEnv = withExceptT GitError $ do
  repoPath <- syncRepo gitRepoDir url
  Git.ensureCheckout repoPath (fromMaybe (Git.Branch "master") branch) checkoutPath
  Kube.getDifferingImages <$> loadEnv sourceEnv <*> loadEnv targetEnv
  where
    checkoutPath = gitRepoDir </> "config-repo"
    loadEnv envPath = Kube.loadEnvFromDisk (checkoutPath </> envPath)

-- | Get the Git revision corresponding to a particular label. Error if we
-- can't figure it out.
labelToRevision :: MonadError Error m => Config.ImageConfig Config.PolicyConfig -> Kube.ImageLabel -> m Git.RevSpec
labelToRevision imageConfig label =
  case Config.imageToRevisionPolicy imageConfig of
    Config.Identity -> pure . Git.RevSpec $ label
    Config.Regex regex -> Git.RevSpec . toS <$> note (RegexError regex label) (regexReplace regex (toS label))

-- | Watch for changes to a single file, performing 'action' when it happens.
watchFile :: MonadIO io => WatchManager -> FilePath -> (Event -> IO ()) -> io StopListening
watchFile mgr filePath action = do
  canonicalPath <- liftIO $ canonicalizePath filePath
  let dir = takeDirectory canonicalPath
  liftIO $ watchDir mgr dir ((== canonicalPath) . eventPath) action
