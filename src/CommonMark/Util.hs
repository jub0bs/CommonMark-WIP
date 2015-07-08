module CommonMark.Util
    ( (<++>)
    , isEndOfLineChar
    , isWhiteSpaceChar
    , isUnicodeWhiteSpaceChar
    , isAsciiSpaceChar
    , isNonSpaceChar
    , isTab
    , isBacktick
    , isAsciiPunctuationChar
    , isPunctuationChar
    , isATXHeaderChar
    , stripAsciiSpaces
    , stripATXSuffix
    , detab
    , replaceNullChars
    ) where

import           Control.Applicative                      ( liftA2 )
import           Data.Char                                ( ord )
import           Data.Text                                ( Text )
import qualified Data.Text                     as T
import           Data.CharSet                             ( CharSet )
import qualified Data.CharSet                  as CharSet
import qualified Data.CharSet.Unicode.Category as CharSet ( punctuation
                                                          , space
                                                          )

-- | "Lifted" version of @(++)@.
(<++>) :: (Applicative f) => f [a] -> f [a] -> f [a]
(<++>) = liftA2 (++)

-- A line ending is a newline (U+000A), carriage return (U+000D), or carriage
-- return + newline.
isEndOfLineChar :: Char -> Bool
isEndOfLineChar c = c == '\n' || c == '\r'

-- A whitespace character is a space (U+0020), tab (U+0009), newline (U+000A),
-- line tabulation (U+000B), form feed (U+000C), or carriage return (U+000D).
isWhiteSpaceChar :: Char -> Bool
isWhiteSpaceChar c =    c == ' '
                     || c == '\t'
                     || c == '\n'
                     || c == '\v'
                     || c == '\f'
                     || c == '\r'

-- A unicode whitespace character is any code point in the unicode Zs class,
-- or a tab (U+0009), carriage return (U+000D), newline (U+000A), or form feed
-- (U+000C).
-- (See http://www.unicode.org/Public/UNIDATA/UnicodeData.txt for details.)
isUnicodeWhiteSpaceChar :: Char -> Bool
isUnicodeWhiteSpaceChar c = c `CharSet.member` unicodeWhiteSpaceCharSet

-- The set of unicode whitespace characters.
unicodeWhiteSpaceCharSet :: CharSet
unicodeWhiteSpaceCharSet =
    CharSet.space `CharSet.union` CharSet.fromList "\t\r\n\f"

-- A space is U+0020.
isAsciiSpaceChar :: Char -> Bool
isAsciiSpaceChar c = c == ' '

isTab :: Char -> Bool
isTab = (== '\t')

isBacktick :: Char -> Bool
isBacktick = (== '`')

-- A non-space character is any character that is not a whitespace character.
isNonSpaceChar :: Char -> Bool
isNonSpaceChar = not . isWhiteSpaceChar

-- An ASCII punctuation character is !, ", #, $, %, &, ', (, ), *, +, ,, -, .,
-- /, :, ;, <, =, >, ?, @, [, \, ], ^, _, `, {, |, }, or ~.
isAsciiPunctuationChar :: Char -> Bool
isAsciiPunctuationChar c =
    c `CharSet.member` asciiPunctuationCharSet

asciiPunctuationCharSet :: CharSet
asciiPunctuationCharSet =
    CharSet.fromList "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"

-- A punctuation character is an ASCII punctuation character or anything in
-- the unicode classes Pc, Pd, Pe, Pf, Pi, Po, or Ps.
-- Note: Data.CharSet.punctuation contains "!\"#%&'()*,-./:;?@[\\]_{}".
isPunctuationChar :: Char -> Bool
isPunctuationChar c =
       c `CharSet.member` CharSet.fromList "$+<=>^`|~"
    || c `CharSet.member` CharSet.punctuation

-- | TODO
isATXHeaderChar :: Char -> Bool
isATXHeaderChar c = c == '#'

-- | Remove leading and trailing ASCII spaces from a string.
stripAsciiSpaces :: Text -> Text
stripAsciiSpaces = T.dropAround isAsciiSpaceChar


-- | @stripATXSuffix t@ strips an ATX-header suffix (if any) from @t@.
stripATXSuffix :: Text -> Text
stripATXSuffix t
    | T.null t'                            = t
    | not . isAsciiSpaceChar . T.last $ t' = t
    | otherwise                            = T.init t'
  where
    t' = T.dropWhileEnd isATXHeaderChar  .
         T.dropWhileEnd isAsciiSpaceChar $ t


-- Convert tabs to spaces using a 4-space tab stop;
-- intended to operate on a single line of input.
-- (adapted from Cheapstake.Util)
detab :: Text -> Text
detab = T.concat . pad . T.split (== '\t')
  where
    -- pad :: [Text] -> [Text]
    pad []               = []
    pad [t]              = [t]
    pad (t : ts@(_ : _)) =
        let
          tabw = 4  -- tabstop
          tl   = T.length t
          n    = tl - (tl `rem` tabw) + tabw  {- smallest multiple of
                                                 tabw greater than tl -}
        in T.justifyLeft n ' ' t : pad ts

-- Replace null characters (U+0000) with the replacement character (U+FFFD).
replaceNullChars :: Text -> Text
replaceNullChars = T.map replaceNUL
  where
    replaceNUL c
        |  c == '\NUL' = '\xFFFD'
        | otherwise    = c
