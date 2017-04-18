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

import qualified Control.Logging as Log
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
  :<|> "revisions" :> Get '[HTML] Revisions
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


newtype Revisions = Revisions (Map Kube.ImageName (Maybe (Either Engine.Error [Git.Revision]))) deriving (Show)

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

      renderImage (name, revs) =
        L.h2_ (L.toHtml name) <> renderLogs revs

      renderLogs Nothing =
        L.p_ (L.toHtml ("No repository configured for image" :: Text))
      renderLogs (Just (Left err)) =
        L.pre_ (L.toHtml (show err :: Text))
      renderLogs (Just (Right [])) =
        L.p_ (L.toHtml ("No revisions in range" :: Text))
      renderLogs (Just (Right revs)) =
        L.table_ $ do
          L.tr_ $ do
            L.th_ "SHA-1"
            L.th_ "Date"
            L.th_ "Author"
            L.th_ "Subject"
          foldMap renderRevision revs

      renderRevision Git.Revision{..} =
        L.tr_ $
          L.td_ (L.toHtml abbrevHash) <>
          L.td_ (L.toHtml commitDate) <>
          L.td_ (L.toHtml authorName) <>
          L.td_ (L.toHtml subject)


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
      liftIO $ Log.debug ("Parsed config: " <> show config)
      let configRepo = Engine.configRepo config
      let gitURL = Config.url configRepo
      let srcEnv = Config.sourceEnv configRepo
      let tgtEnv = Config.targetEnv configRepo
      -- TODO: currently using src image as start and tgt image as end of
      -- revision range. This is wrong because source is nomally *ahead* of
      -- target. Swap them around while preserving good variable names.
      let branch = fromMaybe (Git.Branch "master") (Config.branch configRepo)
      diff <- withExceptT to500 $ Engine.compareImages (Config.gitRepoDir appConfig) gitURL branch srcEnv tgtEnv
      let changedImages = getChangedImages diff
      -- TODO: Do this in parallel
      Revisions <$> Map.traverseWithKey (getRevisions config) changedImages
  where
    to500 err = err500 { errBody = show err}

    -- XXX: Silently ignoring things that don't have start or end labels, as
    -- well as images that are only deployed on one environment.
    getChangedImages diff = Map.fromList [ (name, (src, tgt)) | Kube.ImageChanged name (Just src) (Just tgt) <- foldMap identity diff ]

    getRevisions config name (start, end) =
      case Map.lookup name (Engine.images config) of
        Nothing -> pure Nothing
        Just Config.ImageConfig{..} -> do
          result <- liftIO $ runExceptT $ Engine.compareRevisions (Config.gitRepoDir appConfig) imageToRevisionPolicy gitURL start end paths
          pure (Just result)
