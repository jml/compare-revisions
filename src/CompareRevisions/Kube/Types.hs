{-# LANGUAGE DeriveGeneric #-}
module CompareRevisions.Kube.Types
  ( Env
  , KubeObject(..)
  , namespacedName
  , kubeObjectFromValue
  , Namespace
  , Kind
  , Name
  , Image(..)
  , ImageName
  , ImageLabel
  ) where

import Protolude

import qualified Data.Aeson as Aeson
import Data.Aeson (Value(..))
import qualified Data.HashMap.Lazy as HashMap

-- | Here, an environment is a mapping from Kubernetes to their definitions.
type Env = Map KubeObject Value

-- | A Kubernetes object
data KubeObject
  = KubeObject { namespace :: Namespace
               , kind :: Kind
               , name :: Name
               } deriving (Eq, Ord, Show, Generic)

instance Aeson.ToJSON KubeObject

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


-- | A Docker image.
data Image = Image ImageName (Maybe ImageLabel) deriving (Eq, Ord, Show)

-- | Name of a Docker image.
type ImageName = Text

-- | Label of a Docker image.
type ImageLabel = Text
