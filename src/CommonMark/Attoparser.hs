{-# LANGUAGE OverloadedStrings #-}

module CommonMark.Attoparser
    ( endOfLine
    , lines
    , hRule
    , atxHeaderLevel
    , closingHashSequence
    , numberSigns1to6
    , setextHeader
    ) where

import Prelude hiding ( lines, takeWhile )

import Control.Applicative ( (<|>)
                           , (<$)
                           , (<*>))
import Control.Monad ( mzero )
import Data.Text ( Text )
import qualified Data.Text as T

import Data.Attoparsec.Text hiding ( endOfLine )

import CommonMark.Util
import CommonMark.Types
import CommonMark.Combinators

-- | @discard p@ applies action @p@ but discards its result.
discard :: Parser a -> Parser ()
discard p = () <$ p


-- Whitespace / scanner

-- | Parse an ASCII space character.
asciiSpace :: Parser Char
asciiSpace = satisfy isAsciiSpace

-- | Skip /zero/ or more ASCII space characters.
skipAsciiSpaces :: Parser ()
skipAsciiSpaces = discard $ takeWhile isAsciiSpace

-- | Skip /one/ or more ASCII space characters.
skipAsciiSpaces1 :: Parser ()
skipAsciiSpaces1 = discard $ takeWhile1 isAsciiSpace

-- | Skip between /zero/ and /three/ ASCII space characters.
skipAsciiSpaces0to3 :: Parser ()
skipAsciiSpaces0to3 = discard $ takeWhileHi isAsciiSpace 3

-- | Skip /zero/ or more whitespace characters.
whitespace :: Parser ()
whitespace = discard $ takeWhile isWhiteSpaceChar

-- | Skip a CommonMark line ending.
endOfLine :: Parser ()
endOfLine =
        (discard $ string "\r\n")
    <|> (discard $ char '\r')
    <|> (discard $ char '\n')

-- | TODO
line :: Parser Text
line = takeTill isEndOfLineChar

-- |
lines :: Parser [Text]
lines = line `sepBy` endOfLine


-- Blocks

-- | Parse a horizontal rule.
hRule :: Parser Block
hRule = Hrule
    <$  skipAsciiSpaces0to3
    <*  (choice . map hRuleSequence) "*-_"
    <*  endOfInput
    <?> "horizontal rule"
  where
    hRuleSequence c = countOrMore 3 (char c <* skipAsciiSpaces)


-- ATX headers

-- FIXME
atxHeader :: Parser Block
atxHeader = Header
    <$> (skipAsciiSpaces0to3 *> atxHeaderLevel) <*> string "ju"

atxHeaderLevel :: Parser Int
atxHeaderLevel =
       numberSigns1to6
    -- <* NotFollowedByNonWhiteSpaceChar

numberSigns1to6 :: Parser Int
numberSigns1to6 =
       char '#'
    *> (T.length <$> takeWhileLoHi (== '#') 1 6)

-- The optional closing sequence of #s must be preceded by a space and may be
-- followed by spaces only.
closingHashSequence :: Parser ()
closingHashSequence =
       char ' '
    *> takeWhile1 (== '#') *> whitespace *> endOfInput


--- setext headers --

setextHeader :: Parser Block
setextHeader = do
    t   <- setextHeaderFirstLine
    lvl <- setextHeaderUnderLine
    return $! Header lvl t

setextHeaderFirstLine :: Parser Text
setextHeaderFirstLine =
        skipAsciiSpaces0to3
     *> line                  --- FIXME: must contain at least 1 nonspace char)
                              ---        must be indented by at most 3 chars
    <*  endOfLine


-- A setext header underline is a sequence of = characters or a sequence of -
-- characters, with no more than 3 spaces indentation and any number of
-- trailing spaces.
setextHeaderUnderLine :: Parser Int
setextHeaderUnderLine =
        skipAsciiSpaces0to3
     *> setextHeaderLevel
    <*  skipAsciiSpaces
    <*  endOfInput
    <?> "setext-header underline"


-- The header is a level 1 header if = characters are used in the setext
-- header underline, and a level 2 header if - characters are used.
setextHeaderLevel :: Parser Int
setextHeaderLevel =
        1 <$ takeWhile1 (== '=')
    <|> 2 <$ takeWhile1 (== '-')

{-
-- fenced code block
codeFenceSequence :: Parser ()
        atLeastCountChars 3 '`'
    <|> atLeastCountChars 3 '~'
  where
    atLeastCountChars n c = () $> count n (char c) *> takeWhile (== c)

openingCodeFence :: Parser (Maybe InfoString)
openingCodeFence =
-}
