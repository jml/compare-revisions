module CompareRevisions.Kube.Load
  ( loadEnvFromCluster
  , loadEnvFromDisk
  , ProcessError(..)
  ) where

import Protolude

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.Map as Map
import qualified Data.Yaml as Yaml
import System.Directory (listDirectory)
import System.FilePath ((</>), takeExtension)
import System.Posix.Files (getFileStatus, isDirectory)
import System.Process (readProcessWithExitCode)

import CompareRevisions.Kube.Types (Env, KubeObject(..), kubeObjectFromValue)

loadEnvFromDisk :: MonadIO m => FilePath -> m Env
loadEnvFromDisk directory = do
  files <- getFiles directory
  let yamlFiles = [ f | f <- files, takeExtension f == ".yaml" ]
  bytes <- traverse (liftIO . ByteString.readFile) yamlFiles
  let values = mapMaybe Yaml.decode bytes -- ignore files that don't parse to yaml
  pure (Map.fromList (mapMaybe valueToPair values)) -- ignore yaml that doesn't look like kubeobject
  where
    valueToPair v =
      case kubeObjectFromValue v of
        Nothing -> Nothing
        Just kubeObj -> Just (kubeObj, v)

data ProcessError = ProcessError ExitCode ByteString deriving (Eq, Show)

-- | Given a @kubeConfig@ and some @kubeObjects@, fetch their definitions from
-- a cluster.
--
-- If anything goes wrong running the @kubectl@ process at any time, just give
-- up.
loadEnvFromCluster :: Maybe FilePath -> [KubeObject] -> ExceptT ProcessError IO Env
loadEnvFromCluster kubeConfig kubeObjects = do
  bytes <- traverse (getClusterDefinition kubeConfig) kubeObjects
  let objToBytes = zip kubeObjects bytes
  pure (Map.fromList (mapMaybe (traverse (Aeson.decode . toS)) objToBytes))

-- | Load the definition of a Kubernetes object from a cluster.
--
-- Uses @kubectl@ under the hood.
getClusterDefinition :: Maybe FilePath -> KubeObject -> ExceptT ProcessError IO ByteString
getClusterDefinition kubeConfig KubeObject{..} = do
  (exitCode, out, err) <- lift $ readProcessWithExitCode "kubectl" kubectlArgs ""
  case exitCode of
    ExitSuccess -> pure (toS out)
    ExitFailure _ -> throwError (ProcessError exitCode (toS err))
  where
    kubectlArgs =
      map toS [ "get"
              , "-o=json"
              , "--namespace=" <> namespace
              ] <> kubeConfigOption <>
      map toS [ kind
              , name
              ]

    kubeConfigOption =
      case kubeConfig of
        Nothing -> []
        Just kubeConfigPath -> ["--kubeconfig=" <> kubeConfigPath]


-- | Breadth-first traversal of @directory@, yielding all files.
getFiles :: MonadIO io => FilePath -> io [FilePath]
getFiles directory = do
  entries <- liftIO $ listDirectory directory
  let contents = [ directory </> entry | entry <- entries ]
  (dirs, files) <- partitionEithers <$> traverse splitDirectory contents
  filesFromSubdirs <- mconcat <$> traverse getFiles dirs
  pure (files <> filesFromSubdirs)
  where
    -- | If @entry@ is a directory, @Left entry@, otherwise @Right entry@.
    splitDirectory :: MonadIO m => FilePath -> m (Either FilePath FilePath)
    splitDirectory entry = do
      stat <- liftIO $ getFileStatus entry
      pure $ if isDirectory stat
             then Left entry
             else Right entry
