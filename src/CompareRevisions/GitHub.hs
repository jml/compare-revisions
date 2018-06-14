{-# LANGUAGE NamedFieldPuns #-}
-- | Utilities for manipulating Git URLs based on GitHub conventions.
module CompareRevisions.GitHub
  ( Organization
  , RepositoryName
  , Repository(..)
  , websiteURI
  , repositoryFromGitURL
  , findIssues
  ) where

import Protolude

import qualified Data.Attoparsec.Text as Atto
import Data.Either (fromRight)
import qualified Data.Text as Text
import Network.URI (URI(..))
import qualified Network.URI as URI

import qualified CompareRevisions.Git as Git
import qualified CompareRevisions.SCP as SCP

-- | A GitHub organization.
type Organization = Text

-- | The name of a repository.
type RepositoryName = Text

-- | A repository on GitHub.
--
-- Construct directly, or from a Git URL with 'getRepo'.
data Repository
  = Repository
  { organization :: Organization -- ^ The name of the organization.
  , repositoryName :: RepositoryName -- ^ The name of the repository.
  } deriving (Eq, Show)

-- | Get the GitHub organization and repository from a Git URL.
repositoryFromGitURL :: Git.URL -> Maybe Repository
repositoryFromGitURL uri =
  case Text.splitOn "/" (repoPath uri) of
    [org, repo] -> Just $ Repository org repo
    _ -> Nothing

-- | The URL for a repository's web page.
websiteURI :: Repository -> URI
websiteURI Repository{organization, repositoryName} =
  URI.nullURI
  { uriScheme = "https:"
  , uriAuthority = Just URI.URIAuth
    { URI.uriUserInfo = ""
    , URI.uriRegName = "github.com"
    , URI.uriPort = ""
    }
  , uriPath = toS (organization <> "/" <> repositoryName)
  }

-- | Get the path to the repository from the URL. Strips any preceding @/@.
repoPath :: Git.URL -> Text
repoPath uri =
  let path = toS $ case uri of
                     Git.SCP scp -> SCP.getFilePath scp
                     Git.URI url -> uriPath url
      withoutGit = fromMaybe path (Text.stripSuffix ".git" path)
  in fromMaybe withoutGit (Text.stripPrefix "/" withoutGit)

-- | Find references to all the GitHub issues within some text.
--
-- TODO: Will ignore any qualifications before issue references, thus
-- returning unusual results for issues in different repositories.
findIssues :: Text -> [Int]
findIssues text =
  fromRight [] (Atto.parseOnly prParser text)
  where
    prParser = many (many (Atto.notChar '#') *> prRef)
    prRef = Atto.char '#' *> Atto.decimal
