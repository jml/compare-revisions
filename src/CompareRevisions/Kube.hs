{-# LANGUAGE DuplicateRecordFields #-}
module CompareRevisions.Kube
  ( -- * Objects in Kubernetes
    KubeObject(..)
  , namespacedName
    -- * Environments
  , Env
  , loadEnvFromDisk
  , loadEnvFromCluster
  , ProcessError(..)
    -- * Images
  , Image(..)
  , ImageName
  , ImageLabel
  , getClusterDefinition
  , getImages
    -- * Image diffs
  , ImageDiff(..)
  , getDifferingImages
  ) where

import Protolude

import qualified Data.Aeson as Aeson
import Data.Aeson (Value(..))
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
               } deriving (Eq, Ord, Show)

-- | A Kubernetes namespace. e.g. "default".
type Namespace = Text

-- | The kind of thing a Kubernetes object is, e.g. "service".
type Kind = Text

-- | The name of a thing in Kubernetes. e.g. "authfe".
type Name = Text

-- | Given a Kubernetes object definition, return a 'KubeObject'.
kubeObjectFromValue :: Value -> Maybe KubeObject
kubeObjectFromValue value = do
  object <- getObject value
  kind <- getText =<< HashMap.lookup "kind" object
  metadata <- getObject =<< HashMap.lookup "metadata" object
  name <- getText =<< HashMap.lookup "name" metadata
  let namespace = fromMaybe "default" (getText =<< HashMap.lookup "namespace" metadata)
  pure (KubeObject namespace kind name)
  where
    getText (String text) = Just text
    getText _ = Nothing

    getObject (Object obj) = Just obj
    getObject _ = Nothing

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

type ImageName = Text
type ImageLabel = Text

-- | A set of images is map from names of images to optional labels.
type Images = Map ImageName (Maybe ImageLabel)

-- | Get all the names of images within a JSON value.
getImageNames :: Value -> [Text]
getImageNames (Object obj) =
  image <> rest
  where
    image =
      case HashMap.lookup "image" obj of
        Just (String name) -> [name]
        _ -> []
    rest = concatMap getImageNames obj
getImageNames (Array arr) = concatMap getImageNames arr
getImageNames _ = []


-- | Parse an image name.
parseImageName :: Text -> Maybe Image
parseImageName imageName =
  case splitOn ":" imageName of
    [name] -> Just (Image name Nothing)
    [name, label] -> Just (Image name (Just label))
    _ -> Nothing

-- | Get the images from a Kubernetes object definition.
--
-- Because a single definition can have multiple images, we return a map of
-- image name to image label, where the label is optional.
getImages :: Value -> Images
getImages value =
  Map.fromList [ (name, label) | Image name label <- images ]
  where
    images = mapMaybe parseImageName (getImageNames value)


data ImageDiff
  = ImageAdded ImageName (Maybe ImageLabel)
  | ImageChanged ImageName (Maybe ImageLabel) (Maybe ImageLabel)
  | ImageRemoved ImageName (Maybe ImageLabel)
  deriving (Eq, Ord, Show)

compareImages :: Images -> Images -> [ImageDiff]
compareImages source target =
  foreach (Map.toList (mapDiff source target)) $
  \(name, diff) ->
    case diff of
      Added x -> ImageAdded name x
      Changed x y -> ImageChanged name x y
      Removed x -> ImageRemoved name x

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


-- | Here, an environment is a mapping from Kubernetes to their definitions.
type Env = Map KubeObject Value

loadEnvFromDisk :: FilePath -> IO Env
loadEnvFromDisk directory = do
  files <- getFiles directory
  let yamlFiles = [ f | f <- files, takeExtension f == ".yaml" ]
  bytes <- traverse ByteString.readFile yamlFiles
  let values = mapMaybe Yaml.decode bytes -- ignore files that don't parse to yaml
  pure (Map.fromList (mapMaybe valueToPair values)) -- ignore yaml that doesn't look like kubeobject
  where
    valueToPair v =
      case kubeObjectFromValue v of
        Nothing -> Nothing
        Just kubeObj -> Just (kubeObj, v)

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

-- | Given two sets of Kubernetes objects, return the images that differ
-- between them.
getDifferingImages :: Env -> Env -> Map KubeObject [ImageDiff]
getDifferingImages sourceEnv targetEnv =
  Map.mapMaybe getImageDiffs (mapDiff sourceImages targetImages)
  where
    sourceImages = map getImages sourceEnv
    targetImages = map getImages targetEnv

    -- If a Kubernetes object was added, we don't really care about the images
    -- within.
    getImageDiffs (Added _) = Nothing
    -- Likewise, we don't care if an object was removed.
    getImageDiffs (Removed _) = Nothing
    -- If an object was changed, we want to get the changes.
    getImageDiffs (Changed src tgt) = Just (compareImages src tgt)


-- | Breadth-first traversal of @directory@, yielding all files.
getFiles :: FilePath -> IO [FilePath]
getFiles directory = do
  entries <- listDirectory directory
  let contents = [ directory </> entry | entry <- entries ]
  (dirs, files) <- partitionEithers <$> traverse splitDirectory contents
  filesFromSubdirs <- mconcat <$> traverse getFiles dirs
  pure (files <> filesFromSubdirs)
  where
    -- | If @entry@ is a directory, @Left entry@, otherwise @Right entry@.
    splitDirectory :: FilePath -> IO (Either FilePath FilePath)
    splitDirectory entry = do
      stat <- getFileStatus entry
      pure $ if isDirectory stat
             then Left entry
             else Right entry
