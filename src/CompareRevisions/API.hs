{-# LANGUAGE FlexibleContexts #-}
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
import Servant (Server, Handler, errBody, err500)
import Servant.API (Get, JSON, (:<|>)(..), (:>))
import Servant.HTML.Lucid (HTML)

import qualified CompareRevisions.Config as Config
import qualified CompareRevisions.Engine as Engine
import qualified CompareRevisions.Git as Git
import qualified CompareRevisions.Kube as Kube

-- | compare-revisions API definition.
type API
  = "images" :> Get '[HTML, JSON] ImageDiffs
  :<|> "revisions" :> Get '[HTML, JSON] Revisions
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
instance L.ToHtml RootPage where
  toHtml _ =
    L.doctypehtml_ $ do
      L.head_ (L.title_ title)
      L.body_ $ do
        L.h1_ title
        L.ul_ $ do
          L.li_ $ L.a_ [L.href_ "/images"] "Compare images"
          L.li_ $ L.a_ [L.href_ "/revisions"] "Compare revisions"
          L.li_ $ L.a_ [L.href_ "/metrics"] (L.code_ "/metrics")
          L.li_ $ L.a_ [L.href_ "/status"] (L.code_ "/status")
          L.p_ $ do
            "Source code at "
            L.a_ [L.href_ sourceURL] (L.toHtml sourceURL)
   where
     title = "compare-revisions"
     sourceURL = "https://github.com/weaveworks-experiments/compare-revisions"
  toHtmlRaw = L.toHtml


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

instance L.ToHtml ImageDiffs where
  toHtmlRaw = L.toHtml
  toHtml (ImageDiffs diffs) =
    L.doctypehtml_ $ do
      L.head_ (L.title_ title)
      L.body_ $ do
        L.h1_ title
        L.table_ $ do
          L.tr_ $ do
            L.th_ "Image"
            L.th_ "dev"
            L.th_ "prod" -- TODO: Read this from the data structure
          rows
    where
      title = "compare-images"
      rows = mconcat (map (L.tr_ . toRow) flattenedImages)

      flattenedImages = sortOn Kube.getImageName (ordNub (fold diffs))

      toRow (Kube.ImageAdded name label) = nameCell name <> labelCell label <> L.td_ "ADDED"
      toRow (Kube.ImageChanged name oldLabel newLabel) = nameCell name <> labelCell oldLabel <> labelCell newLabel
      toRow (Kube.ImageRemoved name label) = nameCell name <> L.td_ "REMOVED" <> labelCell label

      nameCell = L.td_ . L.toHtml
      labelCell = L.td_ . L.toHtml . fromMaybe "<no label>"


newtype Revisions = Revisions (Map Kube.ImageName (Maybe [Git.Revision])) deriving (Eq, Ord, Show)

instance ToJSON Revisions where
  toJSON (Revisions byImage) = toJSON (map revsToJSON byImage)
    where
      revsToJSON revs = map revToJSON <$> revs
      revToJSON (Git.Revision revText) = revText

instance L.ToHtml Revisions where
  toHtmlRaw = L.toHtml
  toHtml (Revisions logs) =
    L.doctypehtml_ $ do
      L.head_ (L.title_ title)
      L.body_ $ do
        L.h1_ title
        byImage
    where
      title = "compare-images"

      byImage = foldMap renderImage (Map.toAscList logs)

      renderImage (name, Nothing) =
        (L.h2_ (L.toHtml name)) <>
        (L.p_ (L.toHtml ("No revisions found (because of any number of reasons)" :: Text)))
      renderImage (name, (Just revs)) =
        (L.h2_ (L.toHtml name)) <>
        (L.ul_ $ foldMap renderRevision revs)

      renderRevision (Git.Revision text) = L.li_ (L.toHtml text)


-- | compare-revisions API implementation.
server :: Config.AppConfig -> Server API
server appConfig = images appConfig :<|> revisions appConfig :<|> pure RootPage


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
      let branch = fromMaybe (Git.Branch "master") (Config.branch configRepo)
      diff <- withExceptT to500 $ Engine.compareImages (Config.gitRepoDir appConfig) gitURL branch srcEnv tgtEnv
      pure (ImageDiffs diff)
  where
    to500 err = err500 { errBody = show err}


-- TODO: Do this work elsewhere. Have the handler read from a variable that
-- has all the data already there.

-- TODO: Lots of "Maybe" and "Alternate" on this whole path. Would probably be
-- good to have specific errors and bubble at least some of them up to the end
-- user.

-- | Show the revisions that are in one environment but not others.
revisions :: Config.AppConfig -> Handler Revisions
revisions appConfig = do
  result <- liftIO $ Engine.loadConfigFile (Config.configFile appConfig)
  case result of
    Left err -> throwError (to500 err)
    Right config -> do
      let configRepo = Engine.configRepo config
      let gitURL = Config.url configRepo
      let srcEnv = Config.sourceEnv configRepo
      let tgtEnv = Config.targetEnv configRepo
      let branch = fromMaybe (Git.Branch "master") (Config.branch configRepo)
      diff <- withExceptT to500 $ Engine.compareImages (Config.gitRepoDir appConfig) gitURL branch srcEnv tgtEnv
      let changedImages = getChangedImages diff
      -- TODO: Do this in parallel
      Revisions <$> Map.traverseWithKey (getRevisions config) changedImages
  where
    to500 err = err500 { errBody = show err}

    -- XXX: Silently ignoring things that don't have start or end labels, as
    -- well as images that are only deployed on one environment.
    getChangedImages diff = Map.fromList [ (name, (start, end)) | Kube.ImageChanged name (Just start) (Just end) <- foldMap identity diff ]

    getRevisions config name (start, end) =
      case Map.lookup name (Engine.images config) of
        Nothing -> pure Nothing
        Just Config.ImageConfig{..} ->
          withExceptT to500 $ Engine.compareRevisions (Config.gitRepoDir appConfig) imageToRevisionPolicy gitURL start end 
