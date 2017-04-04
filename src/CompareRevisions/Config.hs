{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
module CompareRevisions.Config
  ( Config(..)
  , ConfigRepo(..)
  , Environment(..)
  , ImageConfig(..)
  , PolicyConfig(..)
  , PolicyName
  ) where

import Protolude hiding (Identity)

import Control.Monad.Fail (fail)
import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , Value(..)
  , (.:)
  , defaultOptions
  , genericParseJSON
  , genericToJSON
  )
import Data.Aeson.Types (Options(..), SumEncoding(..), camelTo2, typeMismatch)
import qualified Data.Char as Char

import qualified CompareRevisions.Git as Git
import CompareRevisions.Duration (Duration)

data Config
  = Config
  { configRepo :: ConfigRepo
  , images :: Map ImageName (ImageConfig PolicyName)
  , revisionPolicies :: Map PolicyName PolicyConfig
  } deriving (Eq, Ord, Show, Generic)

configOptions :: Options
configOptions = defaultOptions { fieldLabelModifier = camelTo2 '-' }

instance ToJSON Config where
  toJSON = genericToJSON configOptions

instance FromJSON Config where
  parseJSON = genericParseJSON configOptions

-- TODO: Dedupe with ImageName in Kube
type ImageName = Text
type PolicyName = Text

data ConfigRepo
  = ConfigRepo
    { url :: Git.URL
    , pollInterval :: Duration
    , sourceEnv :: Environment
    , targetEnv :: Environment
    } deriving (Eq, Ord, Show, Generic)

configRepoOptions :: Options
configRepoOptions = defaultOptions { fieldLabelModifier = camelTo2 '-' }

instance ToJSON ConfigRepo where
  toJSON = genericToJSON configRepoOptions

instance FromJSON ConfigRepo where
  parseJSON = genericParseJSON configRepoOptions

data Environment
  = Environment
    { name :: EnvironmentName
    , path :: FilePath
    } deriving (Eq, Ord, Show, Generic)

type EnvironmentName = Text

instance FromJSON Environment
instance ToJSON Environment

data ImageConfig policy
  = ImageConfig
  { gitURL :: Git.URL
  , imageToRevisionPolicy :: policy
  } deriving (Eq, Ord, Show, Generic)

imageConfigOptions :: Options
imageConfigOptions = defaultOptions { fieldLabelModifier = camelTo2 '-' }

instance ToJSON policy => ToJSON (ImageConfig policy) where
  toJSON = genericToJSON imageConfigOptions

instance FromJSON policy => FromJSON (ImageConfig policy) where
  parseJSON = genericParseJSON imageConfigOptions

data PolicyConfig
  = Regex
  { match :: Text  -- XXX: Probably a different type
  , output :: Text  -- XXX: Probably a different type
  }
  | Identity
  deriving (Eq, Ord, Show, Generic)

policyConfigOptions :: Options
policyConfigOptions =
  defaultOptions { constructorTagModifier = map Char.toLower
                 , sumEncoding = TaggedObject "type" "contents"
                 }

instance ToJSON PolicyConfig where
  toJSON = genericToJSON policyConfigOptions

instance FromJSON PolicyConfig where
  parseJSON (Object v) = do
    typ <- v .: "type"
    case typ of
      String "identity" -> pure Identity
      String "regex" -> Regex <$> v .: "match" <*> v .: "output"
      String x -> fail $ "Unrecognized policy type: " <> toS x
      x -> typeMismatch "Policy type name" x
  parseJSON x = typeMismatch "Policy config" x

