{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- | Configuration for compare-revisions.
module CompareRevisions.Config
  (
  -- * YAML files
    Config(..)
  , ConfigRepo(..)
  , Environment(..)
  , ImageConfig(..)
  , PolicyConfig(..)
  , PolicyName
  -- * Command line
  , AppConfig(..)
  , flags
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
import Options.Applicative (Parser, help, long, option, str)

import CompareRevisions.Duration (Duration)
import qualified CompareRevisions.Git as Git


-- | Configuration specific to compare-revisions.
data AppConfig = AppConfig
  { configFile :: FilePath
  , gitRepoDir :: FilePath
  } deriving (Eq, Show)

-- | Command-line flags for specifying the app's configuration.
flags :: Parser AppConfig
flags =
  AppConfig
  <$> option str
        (fold
           [ long "config-file"
           , help "Path to YAML file describing Git repositories to sync."
           ])
  <*> option str
        (fold
           [ long "git-repo-dir"
           , help "Directory to store all the Git repositories in."
           ])

-- | User-specified configuration for compare-revisions.
--
-- This is how the configuration is given in, say, a YAML file.
data Config
  = Config
  { configRepo :: ConfigRepo  -- ^ The repository that has the Kubernetes manifests.
  , images :: Map ImageName (ImageConfig PolicyName)  -- ^ Information about the source code of the images.
  , revisionPolicies :: Map PolicyName PolicyConfig -- ^ How to go from information about images to revisions.
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

-- | The repository with the Kubernetes manifests in it.
data ConfigRepo
  = ConfigRepo
    { url :: Git.URL -- ^ Where to download the repository from
    , pollInterval :: Duration -- ^ How frequently to download it
    , sourceEnv :: Environment -- ^ How to find information about the source environment in the checkout
    , targetEnv :: Environment -- ^ How to find information about the target environment in the checkout
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

