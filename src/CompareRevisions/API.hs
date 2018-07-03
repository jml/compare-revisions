{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
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
import Lucid -- Importing unqualified because Lucid module is designed that way.
import qualified Network.HTTP.Types as HTTP
import Network.URI (URI(..), relativeTo)
import qualified Network.Wai as Wai
import qualified Network.URI as URI
import qualified Options.Applicative as Opt
import Servant (Server, Handler, Application)
import Servant.API (Capture, Get, JSON, QueryParam, (:<|>)(..), (:>), Raw)
import Servant.HTML.Lucid (HTML)
import Servant.Server (ServantErr(..), Tagged(..), err404, err500, hoistServer)
import Servant.Utils.Links (Link, linkURI, safeLink)
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
  = ImagesAPI
  :<|> RevisionsAPI
  :<|> ChangelogAPI
  :<|> StaticAPI
  :<|> RootAPI

type ImagesAPI = "images" :> Get '[HTML, JSON] (Page ImageDiffs)
type RevisionsAPI = "revisions" :> Get '[HTML] (Page RevisionDiffs)
type ChangelogAPI = Capture "environment" Config.EnvironmentName :> "changes" :> QueryParam "start" Time.Day :> Get '[HTML] (Page ChangeLog)
type StaticAPI = "static" :> Raw
type RootAPI = Get '[HTML] (Page RootPage)


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
  makePage "compare-revisions" (safeLink api (Proxy @RootAPI)) (RootPage (externalURL config) envs)

-- | Show how images differ between two environments.
images :: HasCallStack => Engine.ClusterDiffer -> ReaderT Config Handler (Page ImageDiffs)
images differ = do
  imageDiffs <- map (ImageDiffs . map Engine.imageDiffs) . Engine.getCurrentDifferences $ differ
  makePage "compare-images" (safeLink api (Proxy @ImagesAPI)) imageDiffs

-- | Show the revisions that are in one environment but not others.
revisions :: Engine.ClusterDiffer -> ReaderT Config Handler (Page RevisionDiffs)
revisions differ = do
  diff <- Engine.getCurrentDifferences differ
  makePage "compare-revisions" (safeLink api (Proxy @RevisionsAPI)) (RevisionDiffs (Engine.revisionDiffs <$> diff))

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
    Right changelog -> makePage (env <> " :: changelog") (safeLink api (Proxy @ChangelogAPI) env start') (ChangeLog start changelog)


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
  , currentURI :: URI   -- ^ URI of the page the user is currently viewing
  , content :: a  -- ^ The main content
  } deriving (Eq, Show)

-- | Make a standard HTML page in the compare revisions app.
makePage :: MonadReader Config m => Text -> Link -> body -> m (Page body)
makePage title currentLink body = do
  config <- ask
  pure (Page config title (linkURI currentLink) body)


instance ToJSON a => ToJSON (Page a) where
  -- Since `Page` is only wrapping values to provide a standard HTML wrapper,
  -- it makes sense for the JSON implementation to just be the JSON of the
  -- underlying content.
  toJSON Page{content} = toJSON content

instance ToHtml a => ToHtml (Page a) where
  toHtmlRaw = toHtml
  toHtml Page{config, title, currentURI, content} =
    doctypehtml_ $ do
      head_ $ do
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1, shrink-to-fit=no"]
        link_ [rel_ "stylesheet", href_ stylesheetURI]
        title_ (toHtml title)
      body_ $ do
        nav_ [class_ "navbar navbar-expand-lg navbar-dark bg-dark"] $
          div_ [class_ "collapse navbar-collapse"] $
            ul_ [class_ "navbar-nav mr-auto"] $ do
              let baseURL = externalURL config
              forM_ navLinks $ \(link', label) ->
                let destURL = safeHref_ baseURL link'
                in if linkURI link' == currentURI
                   then
                     li_ [class_ "nav-item active"] $
                     a_ [class_ "nav-link", destURL] $ do
                       label
                       " "
                       span_ [class_ "sr-only"] "(current)"
                   else li_ [class_ "nav-item"] $ a_ [class_ "nav-link", destURL] label
        main_ [role_ "main"] $ toHtml content
        footer_ [class_ "container"] $ do
          hr_ empty
          p_ $ do
            "Source code at "
            a_ [href_ sourceURL] (toHtml sourceURL)
    where
      sourceURL = "https://github.com/weaveworks-experiments/compare-revisions"
      stylesheetURI = show (linkURI (safeLink api (Proxy @StaticAPI)) `relativeTo` externalURL config) <> "/style.css"
      navLinks =
        [ (safeLink api (Proxy @RootAPI), "Home")
        , (safeLink api (Proxy @ImagesAPI), "Compare images")
        , (safeLink api (Proxy @RevisionsAPI), "Compare revisions")
        , (safeLink api (Proxy @ChangelogAPI) "dev" Nothing, "dev changelog")
        , (safeLink api (Proxy @ChangelogAPI) "prod" Nothing, "prod changelog")
        ]


-- | Represents the root page of the service.
data RootPage = RootPage URI (Map Config.EnvironmentName FilePath) deriving (Eq, Ord, Show)

-- | Very simple root HTML page.
instance ToHtml RootPage where
  toHtmlRaw = toHtml
  toHtml (RootPage externalURL envs) = do
    div_ [class_ "jumbotron"] $ do
      h1_ [class_ "display-3"] "compare-revisions"
      p_ "Compare and contrast the Git revisions on Kubernetes environments"
    div_ [class_ "container"] $
      div_ [class_ "row"] $ do
        div_ [class_ "col-md-6"] $ do
          h2_ "Between environments"
          ul_ $ do
            -- servant 0.14 would allow us to use safeLink', which would let us build a helper to reduce duplication.
            li_ $ a_ [safeHref_ externalURL (safeLink api (Proxy @ImagesAPI))] "Images"
            li_ $ a_ [safeHref_ externalURL (safeLink api (Proxy @RevisionsAPI))] "Revisions"
        div_ [class_ "col-md-6"] $ do
          h2_ "Within environments"
          ul_ $ sequence_ [ li_ $ a_ [safeHref_ externalURL (safeLink api (Proxy @ChangelogAPI) env Nothing)] (toHtml env)
                          | env <- Map.keys envs ]


  -- | Generate an href= attribute that links to something in the API, relative
-- to the external URL.
safeHref_ :: URI -> Link -> Attribute
safeHref_ externalURL link' = href_ (show (linkURI link' `relativeTo` externalURL))

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

instance ToHtml ImageDiffs where
  toHtmlRaw = toHtml
  toHtml (ImageDiffs diffs) =
    div_ [class_ "container", standardPadding] $ do
      h1_ "Compare images"
      p_ [class_ "lead"] "How images differ between environments"
      case diffs of
        Nothing -> noDataYet
        Just diffs' ->
          table_ [class_ "table"] $ do
          tr_ $ do
            th_ [scope_ "col"] "Image"
            th_ [scope_ "col"] "dev"
            th_ [scope_ "col"] "prod" -- TODO: Read the environment names from the data structure, rather than hardcoding
          rows diffs'
    where
      rows diffs' = mconcat (map (tr_ . toRow) (flattenedImages diffs'))
      flattenedImages diffs' = sortOn Kube.getImageName (ordNub (fold diffs'))

      toRow (Kube.ImageAdded name label) = nameCell name <> labelCell label <> td_ "ADDED"
      toRow (Kube.ImageChanged name oldLabel newLabel) = nameCell name <> labelCell oldLabel <> labelCell newLabel
      toRow (Kube.ImageRemoved name label) = nameCell name <> td_ "REMOVED" <> labelCell label

      nameCell = td_ . toHtml
      labelCell = td_ . toHtml . fromMaybe "<no label>"


-- | Used to tell the user we haven't loaded the data yet.
noDataYet :: Monad m => HtmlT m ()
noDataYet = div_ [class_ "alert alert-info", role_ "alert"] "No data yet"

-- | The revisions that differ between images.
--
-- newtype wrapper exists so we can define HTML & JSON views.
newtype RevisionDiffs = RevisionDiffs (Maybe (Map Kube.ImageName (Either Engine.Error (Git.URL, [Git.Revision])))) deriving (Show)

-- TODO: JSON version of Revisions.

instance ToHtml RevisionDiffs where
  toHtmlRaw = toHtml
  toHtml (RevisionDiffs clusterDiff) =
    div_ [class_ "container", standardPadding] $ do
      h1_ "Compare revisions"
      p_ [class_ "lead"] "Git log of how images differ between environments. Shows revisions that are on dev but not on prod."
      case clusterDiff of
        Nothing -> noDataYet
        Just diff -> foldMap renderImage (Map.toAscList diff)
    where
      renderImage (name, revs) =
        h2_ (toHtml name) <> renderLogs revs

      renderLogs (Left (Engine.NoConfigForImage _)) =
        div_ [class_ "alert alert-warning", role_ "alert"] "No repository configured for image"
      renderLogs (Left err) =
        div_ [class_ "alert alert-danger", role_ "alert"] (p_ [class_ "text-monospace"] (toHtml (show err :: Text)))
      renderLogs (Right (_, [])) =
        div_ [class_ "alert alert-info", role_ "alert"] "No revisions in range"
      renderLogs (Right (_, revs)) =
        table_ [class_ "table"] $ do
          void $ tr_ $ do
            void $ th_ [scope_ "col"] "SHA-1"
            void $ th_ [scope_ "col"] "Date"
            void $ th_ [scope_ "col"] "Author"
            void $ th_ [scope_ "col"] "Subject"
          foldMap renderRevision revs

      renderRevision rev@Git.Revision{..} =
        tr_ $
          td_ (code_ (toHtml (Git.abbrevHash rev))) <>
          td_ (toHtml (formatDateAndTime commitDate)) <>
          td_ (toHtml authorName) <>
          td_ (toHtml subject)


data ChangeLog
  = ChangeLog
  { startDate :: Time.Day
  , changelog :: Map Kube.ImageName (Either Engine.Error (Git.URL, [Git.Revision]))
  } deriving (Show)

instance ToHtml ChangeLog where
  toHtmlRaw = toHtml
  toHtml ChangeLog{startDate, changelog} = do
    div_ [class_ "container", standardPadding] $ do
      h1_ "Changelog"
      p_ [class_ "lead"] $ do
        "How the environment has changed since "
        toHtml (formatDate startDate)
      p_ ("Change with " <> code_ "?start=YYYY-MM-DD")
    let (thisWeek, lastWeek, rest) = groupedChanges
    div_ [class_ "container"] $
      case thisWeek of
        [] -> div_ [class_ "alert alert-info"] "No changes in range"
        revs -> do
          h2_ $ do
            "This week "
            span_ [class_ "badge badge-primary"] (toHtml (show (length revs) :: Text))
          renderRevisions revs
    div_ [class_ "container"] $
      case lastWeek of
        [] -> pass
        revs -> do
          h2_ $ do
            "Last week "
            span_ [class_ "badge badge-primary"] (toHtml (show (length revs) :: Text))
          renderRevisions revs
    div_ [class_ "container"] $
      case rest of
        [] -> pass
        revs -> do
          h2_ $ do
            "Earlier "
            span_ [class_ "badge badge-primary"] (toHtml (show (length revs) :: Text))
          renderRevisions revs
    where
      groupedChanges =
        case groupByWeek allChanges of
          [] -> ([], [], [])
          [thisWeek] -> (thisWeek, [], [])
          [thisWeek, lastWeek] -> (thisWeek, lastWeek, [])
          thisWeek:lastWeek:rest -> (thisWeek, lastWeek, mconcat rest)
      allChanges = sortOn (Down . Git.commitDate . snd) (Map.keys (flattenChangelog changelog))
      groupByWeek = List.groupBy ((==) `on` (\(y, w, _) -> (y, w)) . WeekDate.toWeekDate . Time.utctDay . Git.commitDate . snd)
      formatDate = Time.formatTime Time.defaultTimeLocale (Time.iso8601DateFormat Nothing)
      renderRevisions = foldMap (uncurry renderChangelogRevision)


-- | The standard padding at the top of every non-jumbotron page.
--
-- Used to ensure the content doesn't immediately run on to the navbar.
standardPadding :: Attribute
standardPadding = style_ "margin-top: 2rem"

-- XXX: Is there a better return type for this?
-- | Render a Git revision, intended to be part of a changelog, as HTML
--
-- Idea is to show everything we can about who made it and why, so that people
-- reviewing it can gauge its user impact.
renderChangelogRevision
  :: Monad m
  => Git.URL  -- ^ The URL of the Git repository that this revision is from
  -> Git.Revision  -- ^ The revision to render
  -> HtmlT m ()
renderChangelogRevision gitUri Git.Revision{commitDate, authorName, subject, body} = do
  let gitHubRepo = GitHub.repositoryFromGitURL gitUri
  p_ $
    div_ [class_ "card"] $
      div_ [class_ "card-body"] $ do
        h5_ [class_ "card-title"] (toHtml subject)
        h6_ [class_ "card-subtitle mb-2 text-muted"] $ do
          toHtml (authorName <> ", committed on " <> formatShortDate commitDate <> " to ")
          case gitHubRepo of
            Just repo -> renderRepoURL repo
            Nothing -> toHtml (Git.toText gitUri)
        case body of
          Nothing -> pass
          Just body' -> pre_ (code_ (toHtml body'))
        case gitHubRepo of
          Just repo -> do
            mapM_ (linkToIssue repo) (GitHub.findIssues subject)
            " "
          Nothing -> pass
  where
    formatShortDate = toS . Time.formatTime Time.defaultTimeLocale "%e %b"

    renderRepoURL repo =
      let uri = GitHub.websiteURI repo
      in a_ [href_ (show uri)] $
         toHtml $ repoShortName repo

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
      in a_ [class_ "card-link", href_ (show issueURI)] (toHtml issueRef)


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
