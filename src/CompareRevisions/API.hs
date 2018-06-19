{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}

-- | API definition for compare-revisions.
module CompareRevisions.API
  ( API
  , api
  , server
  , Config
  , flags
  ) where

import Protolude hiding (diff)

import Data.Aeson (ToJSON(..))
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Time as Time
import qualified Data.Time.Calendar.WeekDate as WeekDate
import qualified Lucid as L
import qualified Network.HTTP.Types as HTTP
import Network.URI (URI(..), parseRelativeReference, relativeTo, uriToString)
import qualified Network.Wai as Wai
import qualified Network.URI as URI
import qualified Options.Applicative as Opt
import Servant (Server, Handler, Application)
import Servant.API (Capture, Get, JSON, QueryParam, (:<|>)(..), (:>), Raw)
import Servant.HTML.Lucid (HTML)
import Servant.Server (ServantErr(..), Tagged(..), err404, err500, hoistServer)
import Servant.Utils.Links (linkURI, safeLink)
import Servant.Utils.StaticFiles (serveDirectoryWebApp)

import qualified CompareRevisions.Config as Config
import qualified CompareRevisions.Engine as Engine
import qualified CompareRevisions.Git as Git
import qualified CompareRevisions.GitHub as GitHub
import qualified CompareRevisions.Kube as Kube


data Config
  = Config
  { externalURL :: URI  -- ^ Publicly visible base URL of the service, used for making links.
  , staticDir :: Maybe FilePath  -- ^ Directory containing static resources
  } deriving (Eq, Show)

flags :: Opt.Parser Config
flags =
  Config
  <$> Opt.option
        (Opt.eitherReader parseURI)
        (fold
           [ Opt.long "external-url"
           , Opt.help "Publicly visible base URL of the service."
           ])
  <*> optional
      (Opt.option
         Opt.str
        (fold
          [ Opt.long "static-dir"
          , Opt.help "Path to directory containing static resources."
          ]))
  where
    parseURI = note "Must be an absolute URL" . URI.parseAbsoluteURI


-- | compare-revisions API definition.
type API
  = "images" :> Get '[HTML, JSON] (Page ImageDiffs)
  :<|> "revisions" :> Get '[HTML] (Page RevisionDiffs)
  :<|> Capture "environment" Config.EnvironmentName :> "changes" :> QueryParam "start" Time.Day :> Get '[HTML] (Page ChangeLog)
  :<|> "static" :> Raw
  :<|> Get '[HTML] (Page RootPage)

-- TODO: Also want to show:
--  - current config
--  - when config last updated
--  - browsing config repo?

-- | Value-level representation of API.
api :: Proxy API
api = Proxy

-- | API implementation.
server :: Config -> Engine.ClusterDiffer -> Server API
server config clusterDiffer
  = hoistServer api (`runReaderT` config)
    ( images clusterDiffer
      :<|> revisions clusterDiffer
      :<|> changes clusterDiffer
      -- servant 0.14 makes this 'tagged' dance unnecessary.
      :<|> Tagged (unTagged (serveStaticDir (staticDir config)))
      :<|> rootPage clusterDiffer )

-- | Serve a static directory, if we're given one. If not, serve an endpoint
-- that returns 404 for everything.
serveStaticDir :: Maybe FilePath -> Tagged Handler Application
serveStaticDir (Just path) = serveDirectoryWebApp path
serveStaticDir _ = Tagged $ \_ respond -> respond $ Wai.responseLBS HTTP.status404 [] "No static resources supplied."

-- | The root page of the application. Links to everything else.
rootPage :: HasCallStack => Engine.ClusterDiffer -> ReaderT Config Handler (Page RootPage)
rootPage differ = do
  config <- ask
  envs <- findEnvironments <$> Engine.getConfig differ
  makePage "compare-revisions" (RootPage (externalURL config) envs)

-- | Show how images differ between two environments.
images :: HasCallStack => Engine.ClusterDiffer -> ReaderT Config Handler (Page ImageDiffs)
images differ = do
  imageDiffs <- map (ImageDiffs . map Engine.imageDiffs) . Engine.getCurrentDifferences $ differ
  makePage "compare-images" imageDiffs

-- | Show the revisions that are in one environment but not others.
revisions :: Engine.ClusterDiffer -> ReaderT Config Handler (Page RevisionDiffs)
revisions differ = do
  diff <- Engine.getCurrentDifferences differ
  makePage "compare-revisions" (RevisionDiffs (Engine.revisionDiffs <$> diff))

-- | Show recent changes to a particular cluster.
--
-- Probably want this to take the following parameters:
--   - the cluster to look at
--   - the start date for changes (and default to something like 2 weeks ago)
--   - the end date for changes (defaulting to 'now')
--
-- Initial version should not take end date (YAGNI).
--
-- Then use that to:
--   - find the configuration for the cluster
--   - check out a version for the start date
--   - (check out a version for the end date)
--   - Use Kube.getDifferingImages to find the images that differ
--   - Use Engine.compareRevisions to find the git revisions
--   - Organize this information reverse chronologically,
--     probably not even grouped by images.
changes :: Engine.ClusterDiffer -> Config.EnvironmentName -> Maybe Time.Day -> ReaderT Config Handler (Page ChangeLog)
changes differ env start' = do
  envs <- findEnvironments <$> Engine.getConfig differ
  envPath <- case Map.lookup env envs of
    Nothing -> throwError $ err404 { errBody = "No such environment: " <> toS env }
    Just envPath -> pure envPath
  start <- case start' of
    Nothing -> do
      now <- liftIO Time.getCurrentTime
      let today = Time.utctDay now
      -- TODO: Would like to pick the last Sunday that gives us two whole weeks.
      pure (Time.addDays (-14) today)
    Just start'' -> pure start''
  changelog' <- liftIO . runExceptT $ Engine.loadChanges differ envPath start
  case changelog' of
    Left err -> throwError $ err500 { errBody = "Could not load config repo: " <> show err }
    Right changelog -> makePage (env <> " :: changelog") (ChangeLog start changelog)


-- | Find all of the environments in our configuration.
findEnvironments :: Config.ValidConfig -> Map Config.EnvironmentName FilePath
findEnvironments cfg = Map.fromList [(Config.name env, Config.path env) | env <- envs]
  where
    envs = [Config.sourceEnv repo, Config.targetEnv repo]
    repo = Config.configRepo cfg

-- | A standard HTML page in the compare-revisions app.
data Page a
  = Page
  { config :: Config -- ^ The configuration for the app
  , title :: Text  -- ^ The title of the page
  , content :: a  -- ^ The main content
  } deriving (Eq, Show)

-- | Make a standard HTML page in the compare revisions app.
makePage :: MonadReader Config m => Text -> body -> m (Page body)
makePage title body = do
  config <- ask
  pure (Page config title body)

instance ToJSON a => ToJSON (Page a) where
  -- Since `Page` is only wrapping values to provide a standard HTML wrapper,
  -- it makes sense for the JSON implementation to just be the JSON of the
  -- underlying content.
  toJSON Page{content} = toJSON content

instance L.ToHtml a => L.ToHtml (Page a) where
  toHtmlRaw = L.toHtml
  toHtml Page{config, title, content} =
    L.doctypehtml_ $ do
      L.head_ $ do
        L.meta_ [L.name_ "viewport", L.content_ "width=device-width, initial-scale=1, shrink-to-fit=no"]
        L.link_ [L.rel_ "stylesheet", L.href_ stylesheetURI]
        L.title_ (L.toHtml title)
      L.body_ $ do
        L.h1_ (L.toHtml title)
        L.toHtml content
        L.p_ $ do
          "Source code at "
          L.a_ [L.href_ sourceURL] (L.toHtml sourceURL)
    where
      sourceURL = "https://github.com/weaveworks-experiments/compare-revisions"
      stylesheetURI = show (linkURI (safeLink api staticResources) `relativeTo` externalURL config) <> "/style.css"
      staticResources = Proxy :: Proxy ("static" :> Raw)


-- | Represents the root page of the service.
data RootPage = RootPage URI (Map Config.EnvironmentName FilePath) deriving (Eq, Ord, Show)

-- | Very simple root HTML page.
instance L.ToHtml RootPage where
  toHtmlRaw = L.toHtml
  toHtml (RootPage externalURL envs) = do
    L.h2_ "Between environments"
    L.ul_ $ do
      L.li_ $ L.a_ [L.href_ (getURL "images")] "Images"
      L.li_ $ L.a_ [L.href_ (getURL "revisions")] "Revisions"
    L.h2_ "Within environments"
    L.ul_ $ sequence_ [ L.li_ $ L.a_ [L.href_ (getURL (toS env <> "/changes"))] (L.toHtml env)
                      | env <- Map.keys envs ]
    L.h2_ "Ops"
    L.ul_ $
      L.li_ $ L.a_ [L.href_ (getURL "metrics")] (L.code_ "metrics")
    where
      -- TODO: Use safeLink
      getURL path =
        case parseRelativeReference path of
          Nothing -> panic $ toS path <> " is not a valid relative URI"
          Just path' -> toS (uriToString identity (path' `relativeTo` externalURL) "")

-- | The images that differ between Kubernetes objects.
-- Newtype wrapper is to let us provide nice HTML.
newtype ImageDiffs = ImageDiffs (Maybe (Map Kube.KubeID [Kube.ImageDiff])) deriving (Eq, Ord, Show, Generic)

instance ToJSON ImageDiffs where
  -- I *think* we can't get a default instance because Aeson cowardly refuses
  -- to objects where the keys are objects.
  toJSON (ImageDiffs diffs) = toJSON (Map.fromList . map reshapeKeys . Map.toList <$> diffs)
    where
      reshapeKeys (kubeID, diff) =
        ( Kube.namespacedName kubeID
        , Map.fromList [ ("kind" :: Text, toJSON (Kube.kind kubeID))
                       , ("diff", toJSON diff)
                       ]
        )

instance L.ToHtml ImageDiffs where
  toHtmlRaw = L.toHtml
  toHtml (ImageDiffs diffs) =
    case diffs of
      Nothing -> L.p_ (L.toHtml ("No data yet" :: Text))
      Just diffs' ->
        L.table_ $ do
          L.tr_ $ do
            L.th_ "Image"
            L.th_ "dev"
            L.th_ "prod" -- TODO: Read the environment names from the data structure, rather than hardcoding
          rows diffs'
    where
      rows diffs' = mconcat (map (L.tr_ . toRow) (flattenedImages diffs'))
      flattenedImages diffs' = sortOn Kube.getImageName (ordNub (fold diffs'))

      toRow (Kube.ImageAdded name label) = nameCell name <> labelCell label <> L.td_ "ADDED"
      toRow (Kube.ImageChanged name oldLabel newLabel) = nameCell name <> labelCell oldLabel <> labelCell newLabel
      toRow (Kube.ImageRemoved name label) = nameCell name <> L.td_ "REMOVED" <> labelCell label

      nameCell = L.td_ . L.toHtml
      labelCell = L.td_ . L.toHtml . fromMaybe "<no label>"


-- | The revisions that differ between images.
--
-- newtype wrapper exists so we can define HTML & JSON views.
newtype RevisionDiffs = RevisionDiffs (Maybe (Map Kube.ImageName (Either Engine.Error (Git.URL, [Git.Revision])))) deriving (Show)

-- TODO: JSON version of Revisions.

instance L.ToHtml RevisionDiffs where
  toHtmlRaw = L.toHtml
  toHtml (RevisionDiffs clusterDiff) =
    case clusterDiff of
      Nothing -> L.p_ (L.toHtml ("No data yet" :: Text))
      Just diff -> foldMap renderImage (Map.toAscList diff)
    where
      renderImage (name, revs) =
        L.h2_ (L.toHtml name) <> renderLogs revs

      renderLogs (Left (Engine.NoConfigForImage _)) =
        L.p_ (L.toHtml ("No repository configured for image" :: Text))
      renderLogs (Left err) =
        L.pre_ (L.toHtml (show err :: Text))
      renderLogs (Right (_, [])) =
        L.p_ (L.toHtml ("No revisions in range" :: Text))
      renderLogs (Right (_, revs)) =
        L.table_ $ do
          L.tr_ $ do
            L.th_ "SHA-1"
            L.th_ "Date"
            L.th_ "Author"
            L.th_ "Subject"
          foldMap renderRevision revs

      renderRevision rev@Git.Revision{..} =
        L.tr_ $
          L.td_ (L.toHtml (Git.abbrevHash rev)) <>
          L.td_ (L.toHtml (formatDateAndTime commitDate)) <>
          L.td_ (L.toHtml authorName) <>
          L.td_ (L.toHtml subject)


data ChangeLog
  = ChangeLog
  { startDate :: Time.Day
  , changelog :: Map Kube.ImageName (Either Engine.Error (Git.URL, [Git.Revision]))
  } deriving (Show)

instance L.ToHtml ChangeLog where
  toHtmlRaw = L.toHtml
  toHtml ChangeLog{startDate, changelog} = do
    L.p_ ("Since " <> L.toHtml (formatDate startDate))
    L.p_ ("Change with " <> L.code_ "?start=YYYY-MM-DD")
    let (thisWeek, lastWeek, rest) = groupedChanges
    case thisWeek of
      [] -> L.p_ "No changes in range"
      revs -> do
        L.h2_ (L.toHtml ("This week (" <> show (length revs) <> ")" :: Text))
        renderRevisions revs
    case lastWeek of
      [] -> pass
      revs -> do
        L.h2_ (L.toHtml ("Last week (" <> show (length revs) <> ")" :: Text))
        renderRevisions revs
    case rest of
      [] -> pass
      revs -> do
        L.h2_ (L.toHtml ("Earlier (" <> show (length revs) <> ")" :: Text))
        renderRevisions revs
    where
      groupedChanges =
        case groupByWeek allChanges of
          [] -> ([], [], [])
          [thisWeek] -> (thisWeek, [], [])
          [thisWeek, lastWeek] -> (thisWeek, lastWeek, [])
          thisWeek:lastWeek:rest -> (thisWeek, lastWeek, mconcat rest)
      allChanges = reverse (sortOn (Git.commitDate . snd) (Map.keys (flattenChangelog changelog)))
      groupByWeek = List.groupBy ((==) `on` (\(y, w, _) -> (y, w)) . WeekDate.toWeekDate . Time.utctDay . Git.commitDate . snd)
      formatDate = Time.formatTime Time.defaultTimeLocale (Time.iso8601DateFormat Nothing)
      renderRevisions revs =
        L.ul_ [L.class_ "revisions"] $ foldMap (uncurry renderChangelogRevision) revs


-- XXX: Is there a better return type for this?
-- | Render a Git revision, intended to be part of a changelog, as HTML.
--
-- Idea is to show everything we can about who made it and why, so that people
-- reviewing it can gauge its user impact.
renderChangelogRevision
  :: Monad m
  => Git.URL  -- ^ The URL of the Git repository that this revision is from
  -> Git.Revision  -- ^ The revision to render
  -> L.HtmlT m ()
renderChangelogRevision gitUri Git.Revision{commitDate, authorName, subject, body} = do
  let gitHubRepo = GitHub.repositoryFromGitURL gitUri
  L.li_ [L.class_ "revision"] $ do
    void $ L.div_ [L.class_ "subject"] $ do
      case gitHubRepo of
        Just repo -> do
          mapM_ (linkToIssue repo) (GitHub.findIssues subject)
          " "
        Nothing -> pass
      L.b_ (L.toHtml subject)
    void $ L.div_ [L.class_ "by-line"] $ do
      L.toHtml (authorName <> ", committed on " <> formatShortDate commitDate <> " to ")
      case gitHubRepo of
        Just repo -> renderRepoURL repo
        Nothing -> L.toHtml (Git.toText gitUri)
    case body of
      Nothing -> pass
      Just body' -> L.pre_ [L.class_ "body"] $ L.toHtml body'
  where
    formatShortDate = toS . Time.formatTime Time.defaultTimeLocale "%e %b"

    renderRepoURL repo =
      let uri = GitHub.websiteURI repo
      in L.a_ [L.href_ (toS $ uriToString (const "") uri "")] $
         L.toHtml $ repoShortName repo

    -- | How we want a repository to appear on the page. If it's a
    -- @weaveworks@ repository, just refer to it by name. Otherwise, by
    -- @org/name@ if it's a GitHub repo. Otherwise, refuse to guess.
    repoShortName GitHub.Repository{organization, repositoryName} =
      case organization of
        "weaveworks" -> repositoryName
        _ -> organization <> "/" <> repositoryName

    linkToIssue repo issue =
      let uri = GitHub.websiteURI repo
          issueURI = uri { uriPath = uriPath uri <> "/issues/" <> show issue }
          issueRef = repoShortName repo <> "#" <> show issue
      in L.a_ [L.href_ (toS $ uriToString (const "") issueURI "")] (L.toHtml issueRef)


-- | Format a UTC time in the standard way for our HTML.
--
-- This means ISO with numeric timezone.
formatDateAndTime :: Time.UTCTime -> Text
formatDateAndTime = toS . Time.formatTime Time.defaultTimeLocale (Time.iso8601DateFormat (Just "%H:%M:%S%z"))

flattenChangelog
  :: Map Kube.ImageName (Either Engine.Error (Git.URL, [Git.Revision]))
  -> Map (Git.URL, Git.Revision) [Kube.ImageName]
flattenChangelog changelog =
  Map.fromListWith (<>) [((uri, rev), [img]) | (img, Right (uri, revs)) <- Map.toList changelog, rev <- revs]
