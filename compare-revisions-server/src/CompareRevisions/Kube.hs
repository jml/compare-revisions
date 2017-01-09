{-# LANGUAGE DuplicateRecordFields #-}
module CompareRevisions.Kube
  ( -- * Objects in Kubernetes
    KubeObject(..)
  , namespacedName
    -- * Images
  , Image(..)
  , parseImageName
  , getClusterDefinition
  , getImageNames
  ) where

import Protolude

import Data.Aeson (Value(..))
import qualified Data.HashMap.Lazy as HashMap
import Data.Text (splitOn)
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
  = Image { name :: Text
          , label :: Maybe Text
          } deriving (Eq, Ord, Show)


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
