{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
-- | Configuration for compare-revisions.
--
-- There are three "config" structures in this file, which makes it a little
-- confusing:
--
-- - 'AppConfig' -- the flags passed on the command line
-- - 'ConfigFile' -- an intermediate structure outlining the schema of the config file
-- - 'ValidConfig' -- the post-processed config file, guaranteed to be valid.
module CompareRevisions.Config
  (
  -- * YAML files
    ConfigFile
  , ConfigRepo(..)
  , Environment(..)
  , EnvironmentName
  , ImageConfig(..)
  , PolicyConfig(..)
  -- ** Valid configuration from YAML files
  , Error(..)
  , ValidConfig(..)
  , loadConfigFile
  , interpretConfigFile  -- Exported for testing.
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
  , (.:?)
  , (.=)
  , (.!=)
  , defaultOptions
  , genericToJSON
  , object
  , withObject
  )
import Data.Aeson.Types (Options(..), SumEncoding(..), camelTo2, typeMismatch)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
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
import qualified CompareRevisions.SCP as SCP
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
getRepoPath :: FilePath -> Git.URLWithCredentials -> FilePath
getRepoPath gitRepoDir url =
  gitRepoDir </> "repos" </> subdir
  where
    subdir = prefix <> "-" <> suffix
    prefix = show (hash (toS urlText :: ByteString) :: Digest SHA256)
    suffix = toS (snd (Text.breakOnEnd "/" urlText))
    urlText = Git.toText (Git.toURL url)

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
data ConfigFile
  = ConfigFile
  { configRepo :: ConfigRepo'  -- ^ The repository that has the Kubernetes manifests.
  , images :: Map ImageName ImageConfig'  -- ^ Information about the source code of the images.
  , revisionPolicies :: Map PolicyName PolicyConfig -- ^ How to go from information about images to revisions.
  , secrets :: Map SecretName SecretConfig  -- ^ A set of credentials that we can use to fetch our images
  } deriving (Eq, Ord, Show, Generic)

configOptions :: Options
configOptions = defaultOptions { fieldLabelModifier = camelTo2 '-' }

instance ToJSON ConfigFile where
  toJSON = genericToJSON configOptions

instance FromJSON ConfigFile where
  parseJSON (Object obj) =
    ConfigFile
      <$> obj .: "config-repo"
      <*> obj .: "images"
      <*> obj .: "revision-policies"
      <*> (obj .:? "secrets" .!= mempty)
  parseJSON x = typeMismatch "Config" x

-- | The repository with the Kubernetes manifests in it.
data ConfigRepo'
  = ConfigRepo'
    { url' :: Git.URL -- ^ Where to download the repository from
    , branch' :: Maybe Git.Branch -- ^ The branch with the configs in it
    , credentials' :: Maybe SecretName  -- ^ Credentails required to get to the Git repository.
    , pollInterval' :: Duration -- ^ How frequently to download it
    , sourceEnv' :: Environment -- ^ How to find information about the source environment in the checkout
    , targetEnv' :: Environment -- ^ How to find information about the target environment in the checkout
    } deriving (Eq, Ord, Show, Generic)

instance ToJSON ConfigRepo' where
  toJSON ConfigRepo'{url', branch', credentials', pollInterval', sourceEnv', targetEnv'} =
    object $ [ "url" .= url' ]
      <> maybe [] (\b -> ["branch" .= b]) branch'
      <> maybe [] (\c -> ["credentials" .= c]) credentials'
      <> [ "poll-interval" .= pollInterval'
         , "source-env" .= sourceEnv'
         , "target-env" .= targetEnv'
         ]

instance FromJSON ConfigRepo' where
  parseJSON = withObject "ConfigRepo'" $ \obj ->
    ConfigRepo' <$> obj .: "url"
      <*> obj .:? "branch"
      <*> obj .:? "credentials"
      <*> obj .: "poll-interval"
      <*> obj .: "source-env"
      <*> obj .: "target-env"

data Environment
  = Environment
    { name :: EnvironmentName
    , path :: FilePath
    } deriving (Eq, Ord, Show, Generic)

type EnvironmentName = Text

instance FromJSON Environment
instance ToJSON Environment

-- | The configuration for comparing revisions for an image.
--
-- This is what users provide as part of their config file.
data ImageConfig'
  = ImageConfig'
  { -- | The location of the Git repository that builds the image
    gitURL' :: Git.URL
    -- | Credentials required to pull the Git repository, if any.
  , credentials' :: Maybe SecretName
    -- | How we convert from image tags to Git revisions.
  , imageToRevisionPolicy' :: PolicyName
    -- | If provided, a list of paths within the repository that build the
    -- specified image. 'git log' will be restricted to this list when
    -- determining what has changed for an image.
  , paths' :: Maybe [FilePath]
  } deriving (Eq, Ord, Show, Generic)

instance ToJSON ImageConfig' where
  toJSON ImageConfig'{gitURL', credentials', imageToRevisionPolicy', paths'} =
    object $ [ "git-url" .= gitURL' ]
    <> maybe [] (\c -> [ "credentials" .= c ]) credentials'
    <> [ "imageToRevisionPolicy" .= imageToRevisionPolicy' ]
    <> maybe [] (\p -> [ "paths" .= p ]) paths'

instance FromJSON ImageConfig' where
  parseJSON = withObject "ImageConfig'" $ \obj ->
    ImageConfig'
      <$> obj .: "git-url"
      <*> obj .:? "credentials"
      <*> obj .: "image-to-revision-policy"
      <*> obj .:? "paths"


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


-- | The name of a secret used to pull Git repositories.
type SecretName = Text

-- | A pointer to a set of credentials that can be used to pull a Git repository.
data SecretConfig
  = BasicAuthConfig
    { -- | The path to a file that contains the username.
      usernameFile :: FilePath
      -- | The path to a file that contains the password.
    , passwordFile :: FilePath
    }
  | SSHKeyConfig
    { -- | The path to a file that contains the username. If not provided, will default to "git".
      sshUsernameFile :: Maybe FilePath
      -- | The path to the SSH private key.
    , sshKeyFile :: FilePath
    }
  deriving (Eq, Ord, Show)

instance ToJSON SecretConfig where
  toJSON (BasicAuthConfig username password) = object
    [ "type" .= ("basic-auth" :: Text)
    , "username-file" .= username
    , "password-file" .= password
    ]
  toJSON (SSHKeyConfig username keyFile) = object $
    [ "type" .= ("ssh-key" :: Text) ] <> maybe [] (\u -> ["username-file" .= u]) username <> [ "ssh-key-file" .= keyFile ]

instance FromJSON SecretConfig where
  parseJSON (Object v) = do
    typ <- v .: "type"
    case typ of
      String "ssh-key" -> SSHKeyConfig <$> v .:? "username-file" <*> v .: "ssh-key-file"
      String "basic-auth" -> BasicAuthConfig <$> v .: "username-file" <*> v .: "password-file"
      String x -> fail $ "Unrecognized secret type: " <> toS x
      x -> typeMismatch "Secret type name" x
  parseJSON x = typeMismatch "Secret config" x


-- | Given configuration for a secret, actually load the secret.
--
-- We need this so we can specify secrets as files, which works well with
-- Kubernetes Secrets.
loadSecret :: MonadIO io => SecretConfig -> io (Validator ConfigError Git.Secret)
loadSecret BasicAuthConfig{usernameFile, passwordFile} =
  pure <$> (Git.BasicAuth <$> readSecretFile (toS usernameFile) <*> readSecretFile (toS passwordFile))
loadSecret SSHKeyConfig{sshUsernameFile, sshKeyFile} =
  case sshUsernameFile of
    Nothing -> pure . pure $ Git.SSHKey "git" sshKeyFile
    Just filename -> do
      rawUsername <- readSecretFile (toS filename)
      case SCP.makeUsername (toS rawUsername) of
        Nothing -> pure $ throwE (InvalidUsername filename rawUsername)
        Just username -> pure . pure $ Git.SSHKey username sshKeyFile

-- | Read a "secret" from a file that was probably mounted from a Kubernetes secret.
--
-- Strictly loads the file into memory, and splits at the first newline. This
-- latter is an affordance for local development, where it is easy to have a
-- spurious newline at the end of a file.
readSecretFile :: MonadIO io => FilePath -> io ByteString
readSecretFile = map (fst . Char8.break (== '\n')) . liftIO . ByteString.readFile


-- | Configuration we need to compare a cluster.
data ValidConfig
  = ValidConfig
  { configRepo :: ConfigRepo  -- ^ Details of the repository with the Kubernetes manifests.
  , images :: Map ImageName ImageConfig  -- ^ Information about the source code of images.
  } deriving (Eq, Ord, Show)

-- | The repository with the Kubernetes manifests in it.
data ConfigRepo
  = ConfigRepo
    { url :: Git.URLWithCredentials -- ^ Where to download the repository from
    , branch :: Maybe Git.Branch -- ^ The branch with the configs in it
    , pollInterval :: Duration -- ^ How frequently to download it
    , sourceEnv :: Environment -- ^ How to find information about the source environment in the checkout
    , targetEnv :: Environment -- ^ How to find information about the target environment in the checkout
    } deriving (Eq, Ord, Show, Generic)

-- | Full configuration of an image that we wish to compare.
--
-- Normally obtained by 'loadConfigFile'.
data ImageConfig =
  ImageConfig
  { -- | How to get the Git repository that builds the image.
    gitURL :: Git.URLWithCredentials
  , imageToRevisionPolicy :: PolicyConfig
  , paths :: Maybe [FilePath]
  } deriving (Eq, Ord, Show)

-- | Errors that can occur in configs
data Error
  = ParseError ParseException
  | InvalidConfig (NonEmpty ConfigError)
  deriving (Show)

-- | Load a configuration file from disk, producing a valid config.
loadConfigFile :: (MonadIO m, MonadError Error m) => FilePath -> m ValidConfig
loadConfigFile path = do
  cfg <- liftIO $ decodeFileEither path
  config <- liftEither ParseError cfg
  loaded <- runExceptT $ interpretConfigFile config
  liftEither InvalidConfig loaded
  where
    liftEither f = either (throwError . f) pure

-- | Errors that can occur in syntactically valid configurations.
data ConfigError
  = UnknownPolicyName ImageName PolicyName
  | UnknownSecretName SecretName
  | InvalidUsername FilePath ByteString
  | InvalidCredentials Git.CredentialError
  deriving (Eq, Ord, Show)

-- | Interpret a configuration file that has been loaded from disk.
interpretConfigFile :: (MonadIO m, MonadError (NonEmpty ConfigError) m) => ConfigFile -> m ValidConfig
interpretConfigFile config = do
  let ConfigFile{configRepo, images, secrets, revisionPolicies} = config
  scrts <- sequenceA <$> traverse loadSecret secrets
  either throwError pure . runValidator $ do
    loadedSecrets <- scrts
    validateConfig configRepo images loadedSecrets revisionPolicies

-- | Turn a user-specified configuration into a guaranteed valid one.
validateConfig :: ConfigRepo' -> Map ImageName ImageConfig' -> Map SecretName Git.Secret -> Map PolicyName PolicyConfig -> Validator ConfigError ValidConfig
validateConfig repo images secrets policies =
  ValidConfig <$> validatedConfigRepo <*> mappedImages
  where
    validatedConfigRepo = do
      creds <- traverse lookupSecret (credentials' (repo :: ConfigRepo'))
      case Git.applyCredentials (url' (repo :: ConfigRepo')) creds of
        Left err -> throwE (InvalidCredentials err)
        Right urlWithCreds -> pure (ConfigRepo urlWithCreds (branch' repo) (pollInterval' repo) (sourceEnv' repo) (targetEnv' repo))

    mappedImages = Map.traverseWithKey mapImage images

    lookupSecret name = maybe (throwE (UnknownSecretName name)) pure $ Map.lookup name secrets

    mapImage imgName img = do
      policy <- lookupPolicy (imageToRevisionPolicy' img)
      creds <- traverse lookupSecret (credentials' (img :: ImageConfig'))
      case Git.applyCredentials (gitURL' img) creds of
        Left err -> throwE (InvalidCredentials err)
        Right urlWithCreds -> pure (ImageConfig urlWithCreds policy (paths' img))

      where
        lookupPolicy name = maybe (throwE (UnknownPolicyName imgName name)) pure $ Map.lookup name policies
