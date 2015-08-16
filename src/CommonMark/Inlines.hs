-- | Parsers for inlines.

module CommonMark.Inlines
    ( escapedChar
    , numericEntity
    ) where

import Control.Applicative ( (<|>) )
import Data.Bits ( (.|.)
                 , shiftL
                 )
import Data.Char ( chr
                 , isDigit
                 , isHexDigit
                 , ord
                 )
import Data.Text ( Text )
import qualified Data.Text as T
import Data.Attoparsec.Text hiding ( endOfLine )

import CommonMark.Util
import CommonMark.Types
import CommonMark.Combinators

-- Escaped characters

--
escapedChar :: Parser Inline
escapedChar = Escaped <$> (char '\\' *> satisfy isAsciiPunctuationChar)


-- HTML entities

-- Parse a named entity.



-- | Parse a numeric (decimal or hexadecimal) entity. If the integer value
-- obtained is a valid nonzero codepoint, return the corresponding character;
-- otherwise, return the replacement character.
numericEntity :: Parser Text
numericEntity = do
    n <- (string "&#" *> value <*  char ';')
    return $! T.singleton $!
        if 0 < n && n <= 0x10FFFF
        then chr n
        else replacementChar
  where
    value = (xX *> hexadecimal1To8) <|> decimal1To8
    xX    = satisfy (\c -> c == 'x' || c == 'X')

-- | Parse an unsigned decimal integer composed of 1 to 8 digits.
-- Adapted from 'Data.Attoparsec.Text.decimal'.
decimal1To8 :: Parser Int
decimal1To8 = T.foldl' step 0 <$> takeWhileLoHi isDigit 1 8
  where step a c = a * 10 + fromIntegral (ord c - 48)

-- | Parse an unsigned hexadecimal integer composed of 1 to 8 digits.
-- Adapted from 'Data.Attoparsec.Text.hexadecimal'.
hexadecimal1To8 :: Parser Int
hexadecimal1To8 = T.foldl' step 0 <$> takeWhileLoHi isHexDigit 1 8
  where
    step a c | w >= 48 && w <= 57  = (a `shiftL` 4) .|. fromIntegral (w - 48)
             | w >= 97             = (a `shiftL` 4) .|. fromIntegral (w - 87)
             | otherwise           = (a `shiftL` 4) .|. fromIntegral (w - 55)
      where
        w = ord c
