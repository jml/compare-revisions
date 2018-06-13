-- | Utilities for manipulating Git URLs based on GitHub conventions.
module CompareRevisions.GitHub
  ( Organization
  , RepositoryName
  , websiteURI
  , getRepo
  , repoPath
  ) where

import Protolude

import qualified Data.Text as Text
import Network.URI (URI(..))
import qualified Network.URI as URI

import qualified CompareRevisions.Git as Git
import qualified CompareRevisions.SCP as SCP

-- | A GitHub organization.
type Organization = Text

-- | The name of a repository.
type RepositoryName = Text

-- | Assuming a Git URL points to a GitHub repository, generate a URL for that
-- repository's web page.
websiteURI :: Git.URL -> Maybe URI
websiteURI url@(Git.URI uri) =
  Just $ uri { uriPath = toS (repoPath url) }
websiteURI url@(Git.SCP scp) =
  foreach (SCP.getHostname scp) $ \hostname ->
    URI.nullURI
    { uriScheme = "https:"
    , uriAuthority = Just URI.URIAuth
      { URI.uriUserInfo = ""
      , URI.uriRegName = toS (SCP.unHostname hostname)
      , URI.uriPort = ""
      }
    , uriPath = toS (repoPath url)
    }

-- | Get the path to the repository from the URL. Strips any preceding @/@.
repoPath :: Git.URL -> Text
repoPath uri =
  let path = toS $ case uri of
                     Git.SCP scp -> SCP.getFilePath scp
                     Git.URI url -> uriPath url
      withoutGit = fromMaybe path (Text.stripSuffix ".git" path)
  in fromMaybe withoutGit (Text.stripPrefix "/" withoutGit)

-- | Get the GitHub organization and repository from a Git URL.
getRepo :: Git.URL -> Maybe (Organization, RepositoryName)
getRepo uri =
  case Text.splitOn "/" (repoPath uri) of
    [org, repo] -> Just (org, repo)
    _ -> Nothing
