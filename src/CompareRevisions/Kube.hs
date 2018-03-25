{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
module CompareRevisions.Kube
  ( -- * Objects in Kubernetes
    KubeObject(..)
  , namespacedName
    -- * Environments
  , Env
  , loadEnvFromDisk
  , ProcessError(..)
    -- * Images
  , Image(..)
  , ImageName
  , ImageLabel
  , getClusterDefinition
    -- * Image diffs
  , ImageDiff(..)
  , getDifferingImages
  , getImageName
  ) where

import Protolude hiding (diff)

import Control.Monad (fail)
import qualified Data.Aeson as Aeson
import Data.Aeson (Value(..), (.:), (.:?))
import qualified Data.ByteString as ByteString
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Map as Map
import qualified Data.Yaml as Yaml
import Data.Text (splitOn)
import System.Directory (listDirectory)
import System.FilePath ((</>), takeExtension)
import System.Posix.Files (getFileStatus, isDirectory)
import System.Process (readProcessWithExitCode)

-- | A Kubernetes object
data KubeObject
  = KubeObject { namespace :: Namespace
               , kind :: Kind
               , name :: Name
                 -- | All the images mentioned in the object definition.
               , images :: [Image]
               } deriving (Eq, Ord, Show, Generic)

instance Aeson.ToJSON KubeObject

instance Aeson.FromJSON KubeObject where
  parseJSON v = Aeson.withObject "KubeObject" (\obj -> do
    kind <- obj .: "kind"
    metadata <- obj .: "metadata"
    name <- metadata .: "name"
    namespace <- metadata .:? "namespace"
    images <- traverse Aeson.parseJSON (findAll "image" v)
    pure $ KubeObject (fromMaybe "default" namespace) kind name images) v


-- | Find all instances of a given key in a JSON value.
findAll :: Text -> Value -> [Value]
findAll key value =
  case value of
    Object obj -> maybeToList (HashMap.lookup key obj) <> concatMap (findAll key) obj
    Array arr -> concatMap (findAll key) arr
    _ -> []

-- | A Kubernetes namespace. e.g. "default".
type Namespace = Text

-- | The kind of thing a Kubernetes object is, e.g. "service".
type Kind = Text

-- | The name of a thing in Kubernetes. e.g. "authfe".
type Name = Text

-- | The fully qualified name of a Kubernetes object. e.g. "default/authfe".
namespacedName :: KubeObject -> Text
namespacedName KubeObject{..} = namespace <> "/" <> name


data ProcessError = ProcessError ExitCode ByteString deriving (Eq, Show)

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


-- | A Docker image.
data Image
  = Image { name :: ImageName
          , label :: Maybe ImageLabel
          } deriving (Eq, Ord, Show)

instance Aeson.ToJSON Image where
  toJSON (Image name label) = Aeson.toJSON $
    case label of
      Nothing -> name
      Just label' -> name <> ":" <> label'

instance Aeson.FromJSON Image where
  parseJSON = Aeson.withText "Image" $ \img ->
    case splitOn ":" img of
      [name] -> pure (Image name Nothing)
      [name, label] -> pure (Image name (Just label))
      _ -> fail "Too many colons in image name"


type ImageName = Text
type ImageLabel = Text

-- | A set of images is map from names of images to optional labels.
type ImageSet = Map ImageName (Maybe ImageLabel)

-- | Get the images from a Kubernetes object definition.
--
-- Because a single definition can have multiple images, we return a map of
-- image name to image label, where the label is optional.
getImageSet :: KubeObject -> ImageSet
getImageSet kubeObj = Map.fromList [ (name, label) | Image name label <- images kubeObj ]

-- | Possible difference between image sets.
data ImageDiff
  = ImageAdded ImageName (Maybe ImageLabel)
  | ImageChanged ImageName (Maybe ImageLabel) (Maybe ImageLabel)
  | ImageRemoved ImageName (Maybe ImageLabel)
  deriving (Eq, Ord, Show, Generic)

instance Aeson.ToJSON ImageDiff

getImageName :: ImageDiff -> ImageName
getImageName (ImageAdded name _) = name
getImageName (ImageChanged name _ _) = name
getImageName (ImageRemoved name _) = name

-- | A difference in a set of values.
data Diff value
  = Added value
  | Changed value value
  | Removed value
  deriving (Eq, Ord, Show)

-- | Compare two maps.
mapDiff :: (Ord key, Eq value) => Map key value -> Map key value -> Map key (Diff value)
mapDiff source target = Map.fromList (go (Map.toAscList source) (Map.toAscList target))
  where
    go xs [] = map (second Added) xs
    go [] ys = map (second Removed) ys
    go xs@((xKey, xValue):xs') ys@((yKey, yValue):ys') =
      case compare xKey yKey of
        LT -> (xKey, Added xValue):go xs' ys
        EQ
          | xValue == yValue -> go xs' ys'
          | otherwise -> (xKey, Changed xValue yValue):go xs' ys'
        GT -> (yKey, Removed yValue):go xs ys'


-- | Here, an environment is collection of Kubernetes objects.
type Env = [KubeObject]

loadEnvFromDisk :: MonadIO m => FilePath -> m Env
loadEnvFromDisk directory = do
  files <- getFiles directory
  let yamlFiles = [ f | f <- files, takeExtension f == ".yaml" ]
  bytes <- traverse (liftIO . ByteString.readFile) yamlFiles
  pure $ mapMaybe Yaml.decode bytes -- ignore files that don't parse to yaml

-- | Given two sets of Kubernetes objects, return the images that differ
-- between them.
getDifferingImages :: Env -> Env -> Map KubeObject [ImageDiff]
getDifferingImages sourceEnv targetEnv =
  Map.mapMaybe getImageDiffs (mapDiff sourceImages targetImages)
  where
    sourceImages = Map.fromList [(k, getImageSet k) | k <- sourceEnv]
    targetImages = Map.fromList [(k, getImageSet k) | k <- targetEnv]

    -- If a Kubernetes object was added, we don't really care about the images
    -- within.
    getImageDiffs (Added _) = Nothing
    -- Likewise, we don't care if an object was removed.
    getImageDiffs (Removed _) = Nothing
    -- If an object was changed, we want to get the changes.
    getImageDiffs (Changed src tgt) = Just (compareImages src tgt)

    compareImages :: ImageSet -> ImageSet -> [ImageDiff]
    compareImages source target =
      foreach (Map.toList (mapDiff source target)) $
      \(name, diff) ->
        case diff of
          Added x -> ImageAdded name x
          Changed x y -> ImageChanged name x y
          Removed x -> ImageRemoved name x

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
