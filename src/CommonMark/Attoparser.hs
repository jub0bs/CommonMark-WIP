{-# LANGUAGE OverloadedStrings #-}

module CommonMark.Attoparser
    ( endOfLine
    , lines
    , manyCount
    , hRule
    , atxHeaderLevel
    , closingHashSequence
    , numberSigns1to6
    , setextHeader
    ) where

import Prelude hiding ( lines, takeWhile, length )

import Control.Applicative ( (<|>)
                           , (<$)
                           , liftA2, many, Alternative, (<*>))
import Data.Text ( Text )
import qualified Data.Text as T

import Data.Attoparsec.Text hiding ( endOfLine )

import CommonMark.Util
import CommonMark.Types

-- Whitespace / scanner
asciiSpace :: Parser Char
asciiSpace = satisfy isAsciiSpace

asciiSpaces :: Parser ()
asciiSpaces = () <$ takeWhile isAsciiSpace

asciiSpaces1 :: Parser ()
asciiSpaces1 = () <$ takeWhile1 isAsciiSpace

asciiSpaces0to3 :: Parser ()
asciiSpaces0to3 = () <$ upToCountChars 3 isAsciiSpace

whitespace :: Parser ()
whitespace = () <$ takeWhile isWhiteSpaceChar

-- TODO: move to Util.hs (misc helpers)
(<++>) :: Applicative f => f [a] -> f [a] -> f [a]
(<++>) = liftA2 (++)


-- TODO: move to Combinators.hs (parsers not specific to CommonMark)

-- adapted from Cheapskate.Combinators
upToCountChars :: Int -> (Char -> Bool) -> Parser Text
upToCountChars n f = scan 0 $ \n' c -> if n' < n && f c
                                       then (Just $! n' + 1)
                                       else Nothing

-- a generalization of many1 (arbitrary positive integer)
manyCount :: Int -> Parser a -> Parser [a]
manyCount n p = count n p <++> many p

endOfLine :: Parser ()
endOfLine =
        (() <$ string "\r\n")
    <|> (() <$ char '\r')
    <|> (() <$ char '\n')


-------------

line :: Parser Text
line = takeTill isEndOfLineChar

lines :: Parser [Text]
lines = line `sepBy` endOfLine


hRule :: Parser Block
hRule = Hrule
    <$  asciiSpaces0to3
    <*  (choice . map hRuleSequence) "*-_"
    <*  endOfInput
    <?> "horizontal rule"
  where
    hRuleSequence c = manyCount 3 (char c <* asciiSpaces)


-- FIXME
atxHeader :: Parser Block
atxHeader = Header
    <$> (asciiSpaces0to3 *> atxHeaderLevel) <*> string "ju"

atxHeaderLevel :: Parser Int
atxHeaderLevel =
       numberSigns1to6
    -- <* NotFollowedByNonWhiteSpaceChar

numberSigns1to6 :: Parser Int
numberSigns1to6 =
       char '#'
    *> ((\t -> T.length t + 1) <$> upToCountChars 5 (== '#'))

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
        asciiSpaces0to3 
     *> line                  --- FIXME: must contain at least 1 nonspace char)
                              ---        must be indented by at most 3 chars
    <*  endOfLine


-- A setext header underline is a sequence of = characters or a sequence of -
-- characters, with no more than 3 spaces indentation and any number of
-- trailing spaces.
setextHeaderUnderLine :: Parser Int
setextHeaderUnderLine =
        asciiSpaces0to3
     *> setextHeaderLevel
    <*  asciiSpaces
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
