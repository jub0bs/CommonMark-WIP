-- | This module provides useful character-related predicates and special
-- characters.
module CommonMark.Util.Char
    (
    -- * Special characters
      replacementChar

    -- * Basic predicates
    , isAsciiLetter
    , isAsciiAlphaNum

    -- * Whitespace
    , isWhitespace
    , isUnicodeWhitespace

    -- * Punctuation
    , isAsciiPunctuation
    , isPunctuation

    -- * URI schemes
    , isSchemeChar
    , isSchemeSpecificChar

    -- * Email address
    , isAtext
    ) where

import Data.Char ( isAlphaNum, isAscii, isLetter )

import Data.CharSet ( CharSet )
import qualified Data.CharSet                  as CS
import qualified Data.CharSet.Unicode.Category as CS ( punctuation, space )

-- | The replacement character (U+FFFD).
replacementChar :: Char
replacementChar = '\xFFFD'

-- | Selects alphabetic ASCII characters.
isAsciiLetter :: Char -> Bool
isAsciiLetter c = isAscii c && isLetter c

-- | Selects alphanumeric ASCII characters.
isAsciiAlphaNum :: Char -> Bool
isAsciiAlphaNum c = isAscii c && isAlphaNum c

-- | Selects ASCII whitespace characters: space (U+0020), tab (U+0009),
-- newline (U+000A), line tabulation (U+000B), form feed (U+000C), carriage
-- return (U+000D).
isWhitespace :: Char -> Bool
isWhitespace =
    let charset = CS.fromList " \t\n\v\f\r"
    in \c -> c `CS.member` charset

-- | Selects Unicode whitespace character: any code point in the Zs class,
-- tab (U+0009), carriage return (U+000D), newline (U+000A), form feed
-- (U+000C).
isUnicodeWhitespace :: Char -> Bool
isUnicodeWhitespace =
    let charset = CS.space `CS.union` CS.fromList "\t\r\n\f"
    in \c -> c `CS.member` charset

-- | Selects an ASCII punctuation character.
isAsciiPunctuation :: Char -> Bool
isAsciiPunctuation c = c `CS.member` asciiPunctuation

-- | The set of ASCII punctuation characters.
-- See <http://spec.commonmark.org/0.21/#ascii-punctuation-character> for
-- details.
asciiPunctuation :: CharSet
asciiPunctuation = CS.fromList "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"

-- | Selects a punctuation character: ASCII punctuation characters, or
-- anything in the Unicode classes Pc, Pd, Pe, Pf, Pi, Po, or Ps.
isPunctuation :: Char -> Bool
isPunctuation =
    let charset = asciiPunctuation `CS.union` CS.punctuation
    in \c -> c `CS.member` charset

-- | Selects characters that can occur in URI schemes.
isSchemeChar :: Char -> Bool
isSchemeChar c = isAsciiAlphaNum c || c == '-' || c == '.'

-- | Selects characters that are can occur in the scheme-specific part of
-- URIs.
isSchemeSpecificChar :: Char -> Bool
isSchemeSpecificChar c = c /= ' ' && c /= '<' && c /= '>'

-- | Selects characters from the /atext/ production in the grammar of email
-- addresses. See <http://spec.commonmark.org/0.21/#email-address> for more
-- details.
isAtext :: Char -> Bool
isAtext =
    let charset = CS.fromList "!#$%&'*+\\/=?^_`{|}~-"
    in \c -> isAsciiAlphaNum c || c `CS.member` charset
