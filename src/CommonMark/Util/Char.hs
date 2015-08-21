-- | This module provides useful character-related predicates and special
-- characters.
module CommonMark.Util.Char
    (
    -- * Special characters
      nullChar
    , replacementChar
    -- * Basic predicates
    , isAsciiLetter
    , isAsciiAlphaNum
    , isEndOfLineChar
    -- * Whitespace
    , isWhitespaceChar
    , isUnicodeWhitespaceChar
    -- Punctuation
    , isAsciiPunctuationChar
    , isPunctuationChar
    -- * Email address
    , isAtextChar
    ) where

import Data.Char ( isAlphaNum, isAscii, isLetter )

import Data.CharSet ( CharSet )
import qualified Data.CharSet                  as CS
import qualified Data.CharSet.Unicode.Category as CS ( punctuation, space )

-- | The NUL character (U+0000).
nullChar :: Char
nullChar = '\x0'

-- | The replacement character (U+FFFD).
replacementChar :: Char
replacementChar = '\xFFFD'

-- | Selects alphabetic ASCII characters.
isAsciiLetter :: Char -> Bool
isAsciiLetter c = isAscii c && isLetter c

-- | Selects alphanumeric ASCII characters.
isAsciiAlphaNum :: Char -> Bool
isAsciiAlphaNum c = isAscii c && isAlphaNum c

-- | Selects end-of-line characters: newline (U+000A), carriage return
-- (U+000D).
isEndOfLineChar :: Char -> Bool
isEndOfLineChar c = c == '\n' || c == '\r'

-- | Selects ASCII whitespace characters: space (U+0020), tab (U+0009),
-- newline (U+000A), line tabulation (U+000B), form feed (U+000C), carriage
-- return (U+000D).
isWhitespaceChar :: Char -> Bool
isWhitespaceChar =
    let charset = CS.fromList " \t\n\v\f\r"
    in \c -> c `CS.member` charset

-- | Selects Unicode whitespace character: any code point in the Zs class,
-- tab (U+0009), carriage return (U+000D), newline (U+000A), form feed
-- (U+000C).
isUnicodeWhitespaceChar :: Char -> Bool
isUnicodeWhitespaceChar =
    let charset = CS.space `CS.union` CS.fromList "\t\r\n\f"
    in \c -> c `CS.member` charset

-- | Selects an ASCII punctuation character.
isAsciiPunctuationChar :: Char -> Bool
isAsciiPunctuationChar c = c `CS.member` asciiPunctuation

-- | The set of ASCII punctuation characters.
-- See <http://spec.commonmark.org/0.21/#ascii-punctuation-character> for
-- details.
asciiPunctuation :: CharSet
asciiPunctuation = CS.fromList "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"

-- | Selects a punctuation character: ASCII punctuation characters, or
-- anything in the Unicode classes Pc, Pd, Pe, Pf, Pi, Po, or Ps.
isPunctuationChar :: Char -> Bool
isPunctuationChar =
    let charset = asciiPunctuation `CS.union` CS.punctuation
    in \c -> c `CS.member` charset

-- | Selects characters from the _atext_ production in the grammar of email
-- addresses. See <http://spec.commonmark.org/0.21/#email-address> for more
-- details.
isAtextChar :: Char -> Bool
isAtextChar =
    let charset = CS.fromList "!#$%&'*+\\/=?^_`{|}~-"
    in \c -> isAsciiAlphaNum c || c `CS.member` charset
