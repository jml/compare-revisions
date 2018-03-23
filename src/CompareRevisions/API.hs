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

import Protolude hiding (diff)

import Data.Aeson (ToJSON(..))
import qualified Data.Map as Map
import qualified Lucid as L
import Network.URI (URI, parseRelativeReference, relativeTo, uriToString)
import Servant (Server, Handler)
import Servant.API (Get, JSON, (:<|>)(..), (:>))
import Servant.HTML.Lucid (HTML)

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

-- | API implementation.
server :: URI -> Engine.ClusterDiffer -> Server API
server externalURL clusterDiffer
  = images clusterDiffer
  :<|> revisions clusterDiffer
  :<|> pure (RootPage externalURL)

-- | Show how images differ between two environments.
images :: HasCallStack => Engine.ClusterDiffer -> Handler ImageDiffs
images = map (ImageDiffs . map Engine.imageDiffs) . Engine.getCurrentDifferences

-- | Show the revisions that are in one environment but not others.
revisions :: Engine.ClusterDiffer -> Handler Revisions
revisions = map Revisions . Engine.getCurrentDifferences


standardPage :: Monad m => Text -> L.HtmlT m () -> L.HtmlT m ()
standardPage title content =
  L.doctypehtml_ $ do
    L.head_ (L.title_ (L.toHtml title))
    L.body_ $ do
      L.h1_ (L.toHtml title)
      content
      L.p_ $ do
        "Source code at "
        L.a_ [L.href_ sourceURL] (L.toHtml sourceURL)
  where
    sourceURL = "https://github.com/weaveworks-experiments/compare-revisions"

-- | Represents the root page of the service.
newtype RootPage = RootPage URI

-- | Very simple root HTML page.
instance L.ToHtml RootPage where
  toHtmlRaw = L.toHtml
  toHtml (RootPage externalURL) =
    standardPage "compare-revisions" $
      L.ul_ $ do
        L.li_ $ L.a_ [L.href_ (getURL "images")] "Compare images"
        L.li_ $ L.a_ [L.href_ (getURL "revisions")] "Compare revisions"
        L.li_ $ L.a_ [L.href_ (getURL "metrics")] (L.code_ "metrics")
    where
      getURL path =
        case parseRelativeReference path of
          Nothing -> panic $ toS path <> " is not a valid relative URI"
          Just path' -> toS (uriToString identity (path' `relativeTo` externalURL) "")


newtype ImageDiffs = ImageDiffs (Maybe (Map Kube.KubeObject [Kube.ImageDiff])) deriving (Eq, Ord, Show, Generic)

instance ToJSON ImageDiffs where
  -- I *think* we can't get a default instance because Aeson cowardly refuses
  -- to objects where the keys are objects.
  toJSON (ImageDiffs diffs) = toJSON (Map.fromList . map reshapeKeys . Map.toList <$> diffs)
    where
      reshapeKeys (kubeObj, diff) =
        ( Kube.namespacedName kubeObj
        , Map.fromList [ ("kind" :: Text, toJSON (Kube.kind kubeObj))
                       , ("diff", toJSON diff)
                       ]
        )

instance L.ToHtml ImageDiffs where
  toHtmlRaw = L.toHtml
  toHtml (ImageDiffs diffs) = standardPage "compare-images" imageDiffs
    where
      imageDiffs =
        case diffs of
          Nothing -> L.p_ (L.toHtml ("No data yet" :: Text))
          Just diffs' ->
            L.table_ $ do
              L.tr_ $ do
                L.th_ "Image"
                L.th_ "dev"
                L.th_ "prod" -- TODO: Read the environment names from the data structure, rather than hardcoding
              rows diffs'

      rows diffs' = mconcat (map (L.tr_ . toRow) (flattenedImages diffs'))
      flattenedImages diffs' = sortOn Kube.getImageName (ordNub (fold diffs'))

      toRow (Kube.ImageAdded name label) = nameCell name <> labelCell label <> L.td_ "ADDED"
      toRow (Kube.ImageChanged name oldLabel newLabel) = nameCell name <> labelCell oldLabel <> labelCell newLabel
      toRow (Kube.ImageRemoved name label) = nameCell name <> L.td_ "REMOVED" <> labelCell label

      nameCell = L.td_ . L.toHtml
      labelCell = L.td_ . L.toHtml . fromMaybe "<no label>"


newtype Revisions = Revisions (Maybe Engine.ClusterDiff) deriving (Show)

-- TODO: JSON version of Revisions.

instance L.ToHtml Revisions where
  toHtmlRaw = L.toHtml
  toHtml (Revisions clusterDiff) = standardPage "compare-revisions" byImage
    where
      byImage =
        case clusterDiff of
          Nothing -> L.p_ (L.toHtml ("No data yet" :: Text))
          Just diff -> foldMap renderImage (Map.toAscList (Engine.revisionDiffs diff))

      renderImage (name, revs) =
        L.h2_ (L.toHtml name) <> renderLogs revs

      renderLogs (Left (Engine.NoConfigForImage _)) =
        L.p_ (L.toHtml ("No repository configured for image" :: Text))
      renderLogs (Left err) =
        L.pre_ (L.toHtml (show err :: Text))
      renderLogs (Right []) =
        L.p_ (L.toHtml ("No revisions in range" :: Text))
      renderLogs (Right revs) =
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
