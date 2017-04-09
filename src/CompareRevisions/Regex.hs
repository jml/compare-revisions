{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
-- | Regular expression substitution.
--
-- I can't believe I am writing this, but there isn't a substitution function
-- for regular expressions in regex-tdfa:
-- <https://github.com/ChrisKuklewicz/regex-tdfa/issues/4>
module CompareRevisions.Regex
  ( RegexReplace
  , makeRegexReplace
  , regexReplace
    -- * Exported for testing
  , compileReplacement
  ) where

import Protolude

import Control.Monad.Fail (fail)
import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , Value(..)
  , (.:)
  , (.=)
  , object
  )
import Data.Aeson.Types (typeMismatch)
import Data.Array ((!), bounds)
import Data.Attoparsec.ByteString.Char8
  ( Parser
  , parseOnly
  , char
  , decimal
  , endOfInput
  , isDigit
  , satisfy
  , takeWhile1
  )
import qualified Data.ByteString as ByteString
import Text.Regex.TDFA (MatchText, RegexLike, Regex, makeRegexM, match)
import Text.Show (Show(..))

-- | An opaque object representing a regular expression replacement /
-- substitution. i.e. a regular expression with submatches and a string where
-- those submatches are inserted.
data RegexReplace
  = RegexReplace
  { regex :: Regex
  , origRegex :: ByteString
  , replacement :: [Either Int ByteString]
  , origReplacement :: ByteString
  }

instance Eq RegexReplace where
  x == y = origRegex x == origRegex y && origReplacement x == origReplacement y

instance Ord RegexReplace where
  x `compare` y = (origRegex x, origReplacement x) `compare` (origRegex y, origReplacement y)

instance Show RegexReplace where
  show x = "RegexReplace " <> Protolude.show (origRegex x) <> " " <> Protolude.show (origReplacement x)

instance ToJSON RegexReplace where
  toJSON RegexReplace{..} = object [ "match" .= toS @ByteString @Text origRegex
                                   , "output" .= toS @ByteString @Text origReplacement
                                   ]

instance FromJSON RegexReplace where
  parseJSON (Object v) = do
    regex <- v .: "match"
    replacement <- v .: "output"
    case makeRegexReplace (toS regex) (toS (replacement :: Text)) of
      Nothing -> fail $ "Invalid regular expression: " <> regex
      Just re -> pure re
  parseJSON x = typeMismatch "RegexReplace" x


-- | Construct a 'RegexReplace', returning 'Nothing' if the regular expression
-- is invalid.
makeRegexReplace :: ByteString -> ByteString -> Maybe RegexReplace
makeRegexReplace origRegex origReplacement = do
  regex <- makeRegexM origRegex
  let replacement = compileReplacement origReplacement
  pure RegexReplace { regex = regex
                    , origRegex = origRegex
                    , replacement = replacement
                    , origReplacement = origReplacement
                    }

regexReplace :: MonadPlus f => RegexReplace -> ByteString -> f ByteString
regexReplace (RegexReplace regex _ replacement _) = replaceAll regex (replaceSubgroups replacement)


-- | Find all the matches in a bytestring and replace them using the given
-- function.
--
-- Roughly analogous to 'traverse'.
--
-- Derived from <http://stackoverflow.com/questions/9071682/replacement-substition-with-haskell-regex-libraries>
replaceAll :: (RegexLike t ByteString, MonadPlus f) => t -> (MatchText ByteString -> f ByteString) -> ByteString -> f ByteString
replaceAll regex f input = start end
  where
    (_, end, start) = foldl' go (0, input, pure) (match regex input :: [MatchText ByteString])

    go (index, read, write) matchText =
      let (offset, len) = snd (matchText ! 0)
          (skip, start') = ByteString.splitAt (offset - index) read
          (_, remaining) = ByteString.splitAt len start'
          -- TODO: Can probably write this applicatively, reducing the
          -- constraint from MonadPlus to Alternative
          writer x = do
            replacement <- f matchText
            write (skip <> replacement <> x)
      in (offset + len, remaining, writer)


-- | Given a string and a list of matches (e.g. from a regex), replace @\1@
-- with the first match, @\2@ with the second and so forth.
replaceSubgroups :: Alternative f => [Either Int ByteString] -> MatchText ByteString -> f ByteString
replaceSubgroups replacement matches =
  mconcat <$> traverse replaceWithMatches replacement
  where
    replaceWithMatches (Right token) = pure token
    replaceWithMatches (Left i) =
      let (low, high) = bounds matches
      in if low <= i && i <= high
         then pure (fst (matches ! i))
         else empty


-- | Take a "replacement" string (e.g. a string with @\1@ etc. for
-- replacement) and "compile" it into something for easier substitution.
compileReplacement :: HasCallStack => ByteString -> [Either Int ByteString]
compileReplacement replacement =
  -- Can probably rephrase this whole thing to avoid attoparsec, but can't be
  -- bothered.
  case parseOnly (parser <* endOfInput) replacement of
    -- As best as I can tell, this parser should match *all* bytestrings.
    -- Thus, it's a programming error if it cannot, and panicking is
    -- acceptable. -- jml
    Left err -> panic $ "Error parsing " <> Protolude.show replacement <> ": " <> toS err
    Right result -> result
  where
    parser = many ((Left <$> placeholder) <|> (Right <$> (escapedChar <|> notPlaceholder' <|> backslash)))

    placeholder :: Parser Int
    placeholder = char '\\' *> decimal

    notPlaceholder' :: Parser ByteString
    notPlaceholder' = takeWhile1 (/= '\\')

    escapedChar :: Parser ByteString
    escapedChar = do
      back <- char '\\'
      notDigit <- satisfy (not . isDigit)
      pure $ toS [back, notDigit]

    backslash :: Parser ByteString
    backslash = "\\"
