{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Configuration for compare-revisions.
--
-- There are three "config" structures in this file, which makes it a little
-- confusing:
--
-- - 'AppConfig' -- the flags passed on the command line
-- - 'Config' -- an intermediate structure outlining the schema of the config file
-- - 'ValidConfig' -- the post-processed config file, guaranteed to be valid.
module CompareRevisions.Config
  (
  -- * YAML files
  ConfigRepo(..)
  , Environment(..)
  , EnvironmentName
  , ImageConfig(..)
  , PolicyConfig(..)
  -- ** Valid configuration from YAML files
  , Error(..)
  , ValidConfig(..)
  , loadConfigFile
  , validateConfig
  -- * Command line
  , AppConfig(..)
  , flags
  , getRepoPath
  ) where

import Protolude hiding (Identity, hash, option, throwE)

import Control.Monad.Fail (fail)
import Crypto.Hash (hash, Digest, SHA256)
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
import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.Yaml (ParseException, decodeFileEither)
import Options.Applicative (Parser, help, long, option, str)
import System.FilePath ((</>))

import CompareRevisions.Duration (Duration)
import qualified CompareRevisions.Git as Git
import CompareRevisions.Kube (ImageName)
import CompareRevisions.Regex (RegexReplace)
import CompareRevisions.Validator (Validator, runValidator, throwE)


-- | Configuration specific to compare-revisions.
data AppConfig = AppConfig
  { configFile :: FilePath
  , gitRepoDir :: FilePath
  } deriving (Eq, Show)

-- | Where should we store the given Git repository?
--
-- Each repository is stored in a directory underneath the configured
-- 'gitRepoDir'. The name of the directory is the SHA256 of the URL, followed
-- by the URL.
getRepoPath :: FilePath -> Git.URL -> FilePath
getRepoPath gitRepoDir url =
  gitRepoDir </> "repos" </> subdir
  where
    subdir = prefix <> "-" <> suffix
    prefix = show (hash (toS urlText :: ByteString) :: Digest SHA256)
    suffix = toS (snd (Text.breakOnEnd "/" urlText))
    urlText = Git.toText url

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

-- | The repository with the Kubernetes manifests in it.
-- TODO: Credentials are missing
data ConfigRepo
  = ConfigRepo
    { url :: Git.URL -- ^ Where to download the repository from
    , branch :: Maybe Git.Branch -- ^ The branch with the configs in it
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
  , paths :: Maybe [FilePath]
  } deriving (Eq, Ord, Show, Generic)

imageConfigOptions :: Options
imageConfigOptions = defaultOptions { fieldLabelModifier = camelTo2 '-' }

instance ToJSON policy => ToJSON (ImageConfig policy) where
  toJSON = genericToJSON imageConfigOptions

instance FromJSON policy => FromJSON (ImageConfig policy) where
  parseJSON = genericParseJSON imageConfigOptions

-- | The name of a policy for deriving Git revisions from image labels.
type PolicyName = Text

data PolicyConfig
  = Regex RegexReplace
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
      String "regex" -> Regex <$> parseJSON (Object v)
      String x -> fail $ "Unrecognized policy type: " <> toS x
      x -> typeMismatch "Policy type name" x
  parseJSON x = typeMismatch "Policy config" x


-- | Information on all the images.
type ImagePolicies = Map ImageName (ImageConfig PolicyConfig)

-- | Configuration we need to compare a cluster.
data ValidConfig
  = ValidConfig
  { configRepo :: ConfigRepo  -- ^ Details of the repository with the Kubernetes manifests.
  , images :: ImagePolicies  -- ^ Information about the source code of images.
  } deriving (Eq, Ord, Show)

-- | Turn a user-specified configuration into a guaranteed valid one.
validateConfig :: Config -> Either (NonEmpty ConfigError) ValidConfig
validateConfig Config { configRepo = repo, images = images, revisionPolicies = policies } =
  runValidator (ValidConfig repo <$> mappedImages)
  where
    mappedImages = Map.traverseWithKey mapImage images
    mapImage :: ImageName -> ImageConfig PolicyName -> Validator ConfigError (ImageConfig PolicyConfig)
    mapImage imgName img =
      -- XXX: lenses!
      let policyName = imageToRevisionPolicy img
      in case lookupPolicy policyName of
           Nothing -> throwE (UnknownPolicyName imgName policyName)
           Just policy -> pure (img { imageToRevisionPolicy = policy })
    lookupPolicy name = Map.lookup name policies


-- | Errors that can occur in configs
data Error
  = ParseError ParseException
  | InvalidConfig (NonEmpty ConfigError)
  deriving (Show)

-- | Errors that can occur in syntactically valid configurations.
data ConfigError
  = UnknownPolicyName ImageName PolicyName
  deriving (Eq, Ord, Show)

loadConfigFile :: (MonadIO m, MonadError Error m) => FilePath -> m ValidConfig
loadConfigFile path = do
  config <- liftIO (decodeFileEither path)
  case config of
    Left err -> throwError (ParseError err)
    Right config' ->
      case validateConfig config' of
        Left err -> throwError (InvalidConfig err)
        Right result -> pure result
