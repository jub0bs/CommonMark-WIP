{-# LANGUAGE OverloadedStrings #-}

-- | This module provides parsers for /inline/ CommonMark content.
-- See <http://spec.commonmark.org/0.21/#inlines> for more details.
module CommonMark.FrontEnd.Inlines
    (
    -- * Backslash escapes
      escapedChar

    -- * Entities
    , entity
    , namedEntity
    , numericEntity

    -- * Code spans
    , codeSpan

    -- * Autolinks
    , autolink
    , absoluteURI
    , emailAddress
    ) where

import Control.Applicative ( (<|>) )
import Data.Bits ( (.|.), shiftL )
import Data.Char ( chr, isDigit, isHexDigit, ord )
import qualified Data.Sequence as S
import Data.Text ( Text )
import qualified Data.Text as T
import Prelude hiding ( takeWhile )

import Data.Attoparsec.Text ( Parser )
import qualified Data.Attoparsec.Text as A

import CommonMark.Types
import CommonMark.Util.Char
import CommonMark.Util.Entities
import CommonMark.Util.Parsing
import CommonMark.Util.Schemes ( isValidScheme )
import CommonMark.Util.Text

-- | Parses an escaped ASCII punctuation character.
escapedChar :: Parser Inline
escapedChar = Escaped <$> (A.char '\\' *> A.satisfy isAsciiPunctuation)

-- | Parses a HTML (named or numeric) entity.
entity :: Parser Inline
entity = A.char '&' *> (namedEntity <|> numericEntity)

-- | Parses a valid HTML5 named entity stripped of its leading ampersand.
namedEntity :: Parser Inline
namedEntity = do
    t <- asciiWord <* semicolon
    case replaceEntity t of
        Nothing -> failure
        Just t' -> return $ Entity t'
  where
    asciiWord = A.takeWhile1 isAsciiLetter

-- | Parses a numeric (decimal or hexadecimal) entity stripped of its leading
-- ampersand. If the integer value obtained is a valid nonzero codepoint,
-- return the corresponding character; otherwise, return the replacement
-- character.
numericEntity :: Parser Inline
numericEntity = do
    n <- A.char '#' *> value <*  semicolon
    return $ Entity $ T.singleton $
        if 0 < n && n <= 0x10FFFF
        then chr n
        else replacementChar
  where
    value = (xX *> hexadecimal1To8) <|> decimal1To8
    xX    = A.satisfy (\c -> c == 'x' || c == 'X')

-- | Parses a semicolon.
semicolon :: Parser Char
semicolon = A.char ';'

-- | Parses an unsigned decimal integer composed of 1 to 8 digits.
-- Adapted from 'Data.Attoparsec.Text.decimal'.
decimal1To8 :: Parser Int
decimal1To8 = T.foldl' step 0 <$> takeWhileLoHi isDigit 1 8
  where step a c = a * 10 + fromIntegral (ord c - 48)

-- | Parses an unsigned hexadecimal integer composed of 1 to 8 digits.
-- Adapted from 'Data.Attoparsec.Text.hexadecimal'.
hexadecimal1To8 :: Parser Int
hexadecimal1To8 = T.foldl' step 0 <$> takeWhileLoHi isHexDigit 1 8
  where
    step a c | w >= 48 && w <= 57  = (a `shiftL` 4) .|. fromIntegral (w - 48)
             | w >= 97             = (a `shiftL` 4) .|. fromIntegral (w - 87)
             | otherwise           = (a `shiftL` 4) .|. fromIntegral (w - 55)
      where
        w = ord c

-- | Parses one or more backticks (U+0060).
backticks1 :: Parser Text
backticks1 = A.takeWhile1 (== '`')

-- | Parses a code span.
codeSpan :: Parser Inline
codeSpan = do
    ticks <- backticks1
    let end           = A.string ticks <* notFollowedBy (== '`')
        nonBackticks1 = A.takeWhile1 (/= '`')
    contents <- T.concat <$> A.manyTill (nonBackticks1 <|> backticks1) end
    return $ CodeSpan
           $ collapseWhitespace
           $ stripAsciiSpacesAndNewlines contents

-- | Parses a delimiter run.
delimRun :: Parser Text
delimRun = A.takeWhile1 (== '*') <|> A.takeWhile1 (== '_')

-- | Parses an autolink.
autolink :: Parser Inline
autolink = do
    A.char '<'
    absoluteURI' <|> emailAddress'
  where
    absoluteURI' = do
        uri <- absoluteURI
        A.char '>'
        return $ Link (S.singleton $ Textual uri) uri Nothing
    emailAddress' = do
        addr <- emailAddress
        A.char '>'
        return $ Link (S.singleton $ Textual addr)
                      ("mailto:" `T.append` addr)
                      Nothing

-- | Parses an absolute URI.
absoluteURI :: Parser Text
absoluteURI = do
    schm <- candidateScheme
    if isValidScheme schm
    then do
        A.char ':'
        rest <- schemeSpecificPart
        return $! T.concat [schm, T.singleton ':', rest]
    else failure
  where
    candidateScheme    = A.takeWhile1 isSchemeChar
    schemeSpecificPart = A.takeWhile isSchemeSpecificChar

-- | Parses an email address.
emailAddress :: Parser Text
emailAddress =  T.concat <$> sequence [localPart, A.string "@", domain]
  where
    localPart = A.takeWhile1 (\c -> isAtext c || c == '.')
    domain    = T.intercalate "." <$> A.sepBy1 label (A.char '.')
    label     = do
        t <- takeWhileLoHi (\c -> isAsciiAlphaNum c || c == '-') 1 63
        if T.head t == '-' || T.last t == '-'
        then failure
        else return t
