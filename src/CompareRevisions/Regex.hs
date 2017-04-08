{-# LANGUAGE FlexibleContexts #-}
-- | Regular expression substitution.
--
-- I can't believe I am writing this, but there isn't a substitution function
-- for regular expressions in regex-tdfa:
-- <https://github.com/ChrisKuklewicz/regex-tdfa/issues/4>
module CompareRevisions.Regex
  ( regexReplace
    -- * Exported for testing
  , compileReplacement
  ) where

import Protolude

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
import Text.Regex.TDFA (MatchText, RegexLike, match)

-- | Currently, replaces an *entire string* with the replacement, with the
-- placeholders filled by regex groups. Ought to instead only replace the
-- parts of the original string that match the regular expression.
regexReplace :: (RegexLike t ByteString, HasCallStack, MonadPlus f) => t -> ByteString -> ByteString -> f ByteString
regexReplace regex replacement input =
  let reps = compileReplacement replacement
      replace = replaceSubgroups reps
  in replaceAll regex replace input


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
    Left err -> panic $ "Error parsing " <> show replacement <> ": " <> toS err
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
