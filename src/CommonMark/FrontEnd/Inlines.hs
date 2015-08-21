{-# LANGUAGE OverloadedStrings #-}

-- | Parsers for inlines.

module CommonMark.FrontEnd.Inlines
    ( escapedChar
    , entity
    , codeSpan
    , autolink
    ) where

import Control.Applicative ( (<|>) )
import Data.Bits ( (.|.), shiftL )
import Data.Char ( chr, isDigit, isHexDigit, ord )
import qualified Data.Sequence as S
import Data.Text ( Text )
import qualified Data.Text as T
import Prelude hiding ( takeWhile )

import Data.Attoparsec.Text hiding ( endOfLine )

import CommonMark.Types
import CommonMark.Util.Char
import CommonMark.Util.Entities ( entityText )
import CommonMark.Util.Parsing
import CommonMark.Util.Schemes ( isValidScheme )
import CommonMark.Util.Text

-- Escaped characters

-- | Parse an escaped ASCII punctuation character.
escapedChar :: Parser Inline
escapedChar = Escaped <$> (char '\\' *> satisfy isAsciiPunctuation)


-- HTML entities

-- | Parse a HTML (named or numeric) entity.
entity :: Parser Inline
entity = char '&' *> (namedEntity <|> numericEntity)

-- | Parse a valid HTML5 named entity stripped of its leading ampersand.
namedEntity :: Parser Inline
namedEntity = do
    t <- asciiWord <* semicolon
    case entityText t of
        Nothing -> failure
        Just t' -> return $! Entity t'
  where
    asciiWord = takeWhile1 isAsciiLetter

-- | Parse a numeric (decimal or hexadecimal) entity stripped of its leading
-- ampersand. If the integer value obtained is a valid nonzero codepoint,
-- return the corresponding character; otherwise, return the replacement
-- character.
numericEntity :: Parser Inline
numericEntity = do
    n <- char '#' *> value <*  semicolon
    return $! Entity $ T.singleton $
        if 0 < n && n <= 0x10FFFF
        then chr n
        else replacementChar
  where
    value = (xX *> hexadecimal1To8) <|> decimal1To8
    xX    = satisfy (\c -> c == 'x' || c == 'X')

-- | Parse a semicolon.
semicolon :: Parser Char
semicolon = char ';'

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


-- Code spans

-- | Parse one or more backticks
backticks1 :: Parser Text
backticks1 = takeWhile1 (== '`')

-- | Adapted from 'Cheapskate.Inlines'.
codeSpan :: Parser Inline
codeSpan = do
    ticks <- backticks1
    let end           = string ticks <* notFollowedBy (== '`')
        nonBackticks1 = takeWhile1 (/= '`')
    contents <- T.concat <$> manyTill (nonBackticks1 <|> backticks1) end
    return $! CodeSpan
           $  collapseWhitespace
           $  stripAsciiSpacesAndNewlines contents


-- Emphasis and strong emphasis

-- | Delimiter run.
delimRun :: Parser Text
delimRun = takeWhile1 (== '*') <|> takeWhile1 (== '_')


-- Autolinks

-- | Parse an autolink.
autolink :: Parser Inline
autolink = do
    char '<'
    absoluteURI' <|> emailAddress'
  where
    absoluteURI' = do
        uri <- absoluteURI
        char '>'
        return $! Link (S.singleton $! Textual uri) uri Nothing
    emailAddress' = do
        addr <- emailAddress
        char '>'
        return $! Link (S.singleton $! Textual addr)
                       ("mailto:" `T.append` addr)
                       Nothing

-- | Parse an absolute URI.
absoluteURI :: Parser Text
absoluteURI = do
    schm <- candidateScheme
    if isValidScheme schm
    then do
        char ':'
        rest <- schemeSpecificPart
        return $! T.concat [schm, T.singleton ':', rest]
    else failure
  where
    candidateScheme = takeWhile1 (\c -> isAsciiLetter c ||
                                        isDigit c       ||
                                        c == '-'        ||
                                        c == '.'
                                 )
    schemeSpecificPart = takeWhile (\c -> c /= ' ' &&
                                          c /= '<' &&
                                          c /= '>'
                                   )

-- | Parse an email address.
emailAddress :: Parser Text
emailAddress =  T.concat <$> sequence [localPart, string "@", domain]
  where
    localPart = takeWhile1 (\c -> isAtext c || c == '.')
    domain    = T.intercalate "." <$> sepBy1 label (char '.')
    label     = do
        t <- takeWhileLoHi (\c -> isAsciiAlphaNum c || c == '-') 1 63
        if T.head t == '-' || T.last t == '-'
        then failure
        else return t
