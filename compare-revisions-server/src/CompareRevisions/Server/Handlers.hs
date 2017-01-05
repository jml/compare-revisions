{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

-- | Implementation of the compare-revisions API.
module CompareRevisions.Server.Handlers
  ( server
  ) where

import Protolude

import Control.Monad.Except (ExceptT(..))
import Control.Monad.Log (Severity, logInfo)
import Servant (ServantErr, Server, (:<|>)(..), (:~>)(..), enter)
import Text.PrettyPrint.Leijen.Text (Doc, Pretty, text)

import CompareRevisions.API (API, RootPage(..), User(..), Users(..))
import qualified CompareRevisions.Server.Logging as Log

-- | compare-revisions API implementation.
server :: Severity -> Server API
server logLevel = enter (toHandler logLevel) handlers
  where
    handlers = pure RootPage :<|> users

-- | Our custom handler type.
type Handler msg = ExceptT ServantErr (Log.LogM msg IO)

-- | Translate our custom monad into a Servant handler.
--
-- See http://haskell-servant.readthedocs.io/en/stable/tutorial/Server.html#using-another-monad-for-your-handlers
-- for the details.
toHandler
  :: Pretty msg
  => Severity -> (Handler msg :~> ExceptT ServantErr IO)
toHandler logLevel = Nat toHandler'
  where
    toHandler'
      :: Pretty msg
      => Handler msg a -> ExceptT ServantErr IO a
    toHandler' = ExceptT . Log.withLogging logLevel . runExceptT

-- | Example endpoint.
users :: Handler Doc Users
users = do
  logInfo (text "Example of logging")
  pure (Users [User 1 "Isaac" "Newton", User 2 "Albert" "Einstein"])
