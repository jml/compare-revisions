{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

-- | API definition for compare-revisions.
module CompareRevisions.API
  ( API
  , api
  , server
  ) where

import Protolude

import Control.Monad.Except (withExceptT)
import Data.Aeson (ToJSON(..))
import qualified Data.Map as Map
import qualified Lucid as L
import Network.HTTP.Media ((//), (/:))
import Servant (Server, Handler, errBody, err500)
import Servant.API (Accept(..), Get, MimeRender(..), JSON, (:<|>)(..), (:>))

import qualified CompareRevisions.Config as Config
import qualified CompareRevisions.Engine as Engine
import qualified CompareRevisions.Git as Git
import qualified CompareRevisions.Kube as Kube

-- | HTML content type.
data HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

-- | compare-revisions API definition.
type API
  = "images" :> Get '[HTML, JSON] ImageDiffs
  :<|> Get '[HTML] RootPage

-- TODO: Also want to show:
--  - current config
--  - when config last updated
--  - browsing config repo?

-- | Value-level representation of API.
api :: Proxy API
api = Proxy


-- | Represents the root page of the service.
data RootPage = RootPage

-- | Very simple root HTML page. Replace this with your own simple page that
-- describes your API to other developers and sysadmins.
instance MimeRender HTML RootPage where
  mimeRender _ _ =
    L.renderBS $ L.doctypehtml_ $ do
      L.head_ (L.title_ title)
      L.body_ $ do
        L.h1_ title
        L.ul_ $ do
          L.li_ $ L.a_ [L.href_ "/images"] "Compare images"
          L.li_ $ L.a_ [L.href_ "/metrics"] (L.code_ "/metrics")
          L.li_ $ L.a_ [L.href_ "/status"] (L.code_ "/status")
          L.p_ $ do
            "Source code at"
            L.a_ [L.href_ sourceURL] (L.toHtml sourceURL)
   where
     title = "compare-revisions"
     sourceURL = "https://github.com/weaveworks-experiments/compare-revisions"


newtype ImageDiffs = ImageDiffs (Map Kube.KubeObject [Kube.ImageDiff]) deriving (Eq, Ord, Show, Generic)

instance ToJSON ImageDiffs where
  -- I *think* we can't get a default instance because Aeson cowardly refuses
  -- to objects where the keys are objects.
  toJSON (ImageDiffs diffs) = toJSON . Map.fromList . map reshapeKeys . Map.toList $ diffs
    where
      reshapeKeys (kubeObj, diff) =
        ( Kube.namespacedName kubeObj
        , Map.fromList [ ("kind" :: Text, toJSON (Kube.kind kubeObj))
                       , ("diff", toJSON diff)
                       ]
        )

instance MimeRender HTML ImageDiffs where
  mimeRender _ _ =
    L.renderBS $ L.doctypehtml_ $ do
      L.head_ (L.title_ title)
      L.body_ $
        L.h1_ title
    where
      title = "diff"


-- | compare-revisions API implementation.
server :: Config.AppConfig -> Server API
server appConfig = images appConfig :<|> pure RootPage


images :: HasCallStack => Config.AppConfig -> Handler ImageDiffs
images appConfig = do
  result <- liftIO $ Engine.loadConfigFile (Config.configFile appConfig)
  case result of
    Left err -> throwError (to500 err)
    Right config -> do
      let configRepo = Engine.configRepo config
      let gitURL = Config.url configRepo
      let srcEnv = Config.sourceEnv configRepo
      let tgtEnv = Config.targetEnv configRepo
      diff <- withExceptT to500 $ Engine.compareImages (Config.gitRepoDir appConfig) gitURL (Git.RevSpec "origin/master") srcEnv tgtEnv
      pure (ImageDiffs diff)
  where
    to500 err = err500 { errBody = show err}
