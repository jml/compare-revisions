module CompareRevisions.SCP
  ( SCP(..)
  , formatSCP
  , parseSCP
  , Hostname(unHostname)
  , makeHostname
  , Username(unUsername)
  , makeUsername
  ) where

import Protolude hiding (takeWhile)

import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , withText
  )
import Data.Attoparsec.Text
  ( Parser
  , char
  , digit
  , endOfInput
  , letter
  , parseOnly
  , sepBy1
  , takeWhile
  , takeWhile1
  )
import Data.Char (isDigit, isLetter)
import Data.String (IsString(..))
import qualified Data.Text as Text


data SCP = File FilePath
         | RemoteFile Hostname FilePath
         | AuthRemoteFile Username Hostname FilePath
         deriving (Eq, Ord, Show)

instance ToJSON SCP where
  toJSON = toJSON . formatSCP

instance FromJSON SCP where
  parseJSON = withText "SCP destination must be text" parseSCP

formatSCP :: SCP -> Text
formatSCP (File path) = toS path
formatSCP (RemoteFile (Hostname host) path) = host <> ":" <> toS path
formatSCP (AuthRemoteFile (Username user) (Hostname host) path) = user <> "@" <> host <> ":" <> toS path

parseSCP :: Alternative f => Text -> f SCP
parseSCP = hush . parseOnly (scpParser <* endOfInput)
  where
    scpParser = asum [ AuthRemoteFile <$> usernameParser <* "@" <*> hostnameParser <* ":" <*> path
                     , RemoteFile <$> hostnameParser <* ":" <*> path
                     , File <$> path
                     ]
    path = toS <$> takeWhile1 (/= chr 0)


newtype Hostname = Hostname { unHostname :: Text } deriving (Eq, Ord, Show)

makeHostname :: Alternative f => Text -> f Hostname
makeHostname = hush . parseOnly (hostnameParser <* endOfInput)

instance IsString Hostname where
  fromString hostname = let hn = toS hostname in fromMaybe (panic ("Invalid hostname: " <> hn)) (makeHostname hn)

hostnameParser :: Parser Hostname
hostnameParser = Hostname . Text.intercalate "." <$> sepBy1 label (char '.')
  where
    label = do
      firstChar <- letter <|> digit
      body <- takeWhile (anyOf [isDigit, isLetter, (== '-')])
      lastChar <- optional (letter <|> digit)
      pure (Text.singleton firstChar <> body <> maybe mempty Text.singleton lastChar)


newtype Username = Username { unUsername :: Text } deriving (Eq, Ord, Show)

instance IsString Username where
  fromString username = let un = toS username in fromMaybe (panic ("Invalid username: " <> un)) (makeUsername un)

makeUsername :: Alternative f => Text -> f Username
makeUsername = hush . parseOnly (usernameParser <* endOfInput)

usernameParser :: Parser Username
usernameParser = Username <$> (Text.cons <$> (letter <|> char '_') <*> takeWhile (anyOf [isDigit, isLetter, (== '_')]))

-- | True if any of the predicates is satisfied by the given value.
anyOf :: Foldable t => t (a -> Bool) -> a -> Bool
anyOf ps x = any (\f -> f x) ps
