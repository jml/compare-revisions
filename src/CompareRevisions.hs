{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

-- | Serve the API as an HTTP server.
module CompareRevisions
  ( server
  , startApp
  ) where

import Protolude

import Options.Applicative
       (Parser, ParserInfo, execParser, fullDesc, header,
        help, helper, info, long, option, progDesc, str)
import Servant (serve)

import CompareRevisions.API (api, server)

import qualified CompareRevisions.Server as Server



-- | Configuration specific to compare-revisions.
data AppConfig = AppConfig
  { _configFile :: FilePath
  , _gitRepoDir :: FilePath
  } deriving (Eq, Show)

appFlags :: Parser AppConfig
appFlags =
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


data Config = Config AppConfig Server.Config deriving (Eq, Show)

-- | Run the service.
startApp :: IO ()
startApp = do
  Config _appConfig serverConfig <- execParser options
  let app = serve api server
  Server.run serverConfig app

options :: ParserInfo Config
options = info (helper <*> parser) description
  where
    parser = Config <$> appFlags <*> Server.flags

    description =
      fold
        [ fullDesc
        , progDesc "Show how sets of images differ, by revision"
        , header "compare-revisions - webservice to compare k8s clusters"
        ]

