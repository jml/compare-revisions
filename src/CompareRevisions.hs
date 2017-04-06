-- | Entry point for compare-revisions.
module CompareRevisions
  ( startApp
  ) where

import Protolude

import Options.Applicative (ParserInfo, execParser, fullDesc, header, helper, info, progDesc)
import Servant (serve)

import qualified CompareRevisions.API as API

import qualified CompareRevisions.Config as Config
import qualified CompareRevisions.Server as Server

-- | Overall command-line configuration.
data Config = Config Config.AppConfig Server.Config deriving (Eq, Show)

-- | Command-line parser for compare-revisions.
options :: ParserInfo Config
options = info (helper <*> parser) description
  where
    parser = Config <$> Config.flags <*> Server.flags

    description =
      fold
        [ fullDesc
        , progDesc "Show how sets of images differ, by revision"
        , header "compare-revisions - webservice to compare k8s clusters"
        ]

-- | Run the service.
startApp :: IO ()
startApp = do
  Config _appConfig serverConfig <- execParser options
  let app = serve API.api API.server
  Server.run serverConfig app
