-- | 'Char'-related entities.

module CommonMark.Util.Char
    ( isEndOfLineChar
    , isAsciiAlphaNum
    , isWhitespaceChar
    , isUnicodeWhitespaceChar
    , isNonSpaceChar
    , isAsciiPunctuationChar
    , isPunctuationChar
    , isAtextChar
    , replacementChar
    , isAsciiLetter
    ) where

import           Data.Char                           ( isAlphaNum
                                                     , isAscii
                                                     , isLetter
                                                     )

import           Data.CharSet                        ( CharSet )
import qualified Data.CharSet                  as CS
import qualified Data.CharSet.Unicode.Category as CS ( punctuation
                                                     , space
                                                     )
-- A line ending is a newline (U+000A), carriage return (U+000D), or carriage
-- return + newline.
isEndOfLineChar :: Char -> Bool
isEndOfLineChar c = c == '\n' || c == '\r'

isAsciiAlphaNum :: Char -> Bool
isAsciiAlphaNum c = isAscii c && isAlphaNum c

-- A whitespace character is a space (U+0020), tab (U+0009), newline (U+000A),
-- line tabulation (U+000B), form feed (U+000C), or carriage return (U+000D).
isWhitespaceChar :: Char -> Bool
isWhitespaceChar c =    c == ' '
                     || c == '\t'
                     || c == '\n'
                     || c == '\v'
                     || c == '\f'
                     || c == '\r'

-- A unicode whitespace character is any code point in the unicode Zs class,
-- or a tab (U+0009), carriage return (U+000D), newline (U+000A), or form feed
-- (U+000C).
-- (See http://www.unicode.org/Public/UNIDATA/UnicodeData.txt for details.)
isUnicodeWhitespaceChar :: Char -> Bool
isUnicodeWhitespaceChar c = c `CS.member` unicodeWhitespaceCharSet

-- The set of unicode whitespace characters.
unicodeWhitespaceCharSet :: CharSet
unicodeWhitespaceCharSet =
    CS.space `CS.union` CS.fromList "\t\r\n\f"

-- A non-space character is any character that is not a whitespace character.
isNonSpaceChar :: Char -> Bool
isNonSpaceChar = not . isWhitespaceChar

-- An ASCII punctuation character is !, ", #, $, %, &, ', (, ), *, +, ,, -, .,
-- /, :, ;, <, =, >, ?, @, [, \, ], ^, _, `, {, |, }, or ~.
isAsciiPunctuationChar :: Char -> Bool
isAsciiPunctuationChar c =
    c `CS.member` asciiPunctuationCharSet

asciiPunctuationCharSet :: CharSet
asciiPunctuationCharSet =
    CS.fromList "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"

-- A punctuation character is an ASCII punctuation character or anything in
-- the unicode classes Pc, Pd, Pe, Pf, Pi, Po, or Ps.
-- Note: Data.CharSet.punctuation contains "!\"#%&'()*,-./:;?@[\\]_{}".
isPunctuationChar :: Char -> Bool
isPunctuationChar c =
       c `CS.member` CS.fromList "$+<=>^`|~"
    || c `CS.member` CS.punctuation

-- | TODO
isAtextChar :: Char -> Bool
isAtextChar = let charset = CS.fromList "!#$%&'*+\\/=?^_`{|}~-" in
    \c -> isAsciiAlphaNum c || c `CS.member` charset

-- | The replacement character (i.e. the character of codepoint 0xFFFD).
replacementChar :: Char
replacementChar = '\xFFFD'

-- | Self-explanatory.
isAsciiLetter :: Char -> Bool
isAsciiLetter c = isAscii c && isLetter c
