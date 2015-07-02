{-# LANGUAGE OverloadedStrings #-}

module CommonMark.Attoparser
    ( endOfLine
    , line
    , hRule
    , atxHeaderLevel
    , atxHeader
    -- , dropAtxClosingSeq
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
asciiSpace = satisfy isAsciiSpaceChar

-- | Skip /zero/ or more ASCII space characters.
skipAsciiSpaces :: Parser ()
skipAsciiSpaces = discard $ takeWhile isAsciiSpaceChar

-- | Skip /one/ or more ASCII space characters.
skipAsciiSpaces1 :: Parser ()
skipAsciiSpaces1 = discard $ takeWhile1 isAsciiSpaceChar

-- | Skip between /zero/ and /three/ ASCII space characters.
skipAsciiSpaces0to3 :: Parser ()
skipAsciiSpaces0to3 = discard $ takeWhileHi isAsciiSpaceChar 3

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

-- | Parse a horizontal rule. Intended to operate on a single line of input.
hRule :: Parser Block
hRule = Hrule
    <$  skipAsciiSpaces0to3
    <*  (choice . map hRuleSequence) hRuleChars
    <?> "horizontal rule"
  where
    hRuleSequence c = countOrMore 3 (char c <* skipAsciiSpaces)
    hRuleChars      = "*-_"


-- ATX headers

isAtxHeaderChar :: Char -> Bool
isAtxHeaderChar = (== '#')

-- FIXME
atxHeader :: Parser Block
atxHeader =
    skipAsciiSpaces0to3 >>
    atxHeaderLevel      >>= \lvl ->
    peekChar            >>= \maybeChar ->
    case maybeChar of
        Nothing                   -> return $! Header lvl T.empty
        Just c | isNonSpaceChar c -> failure
        _                         ->
            takeText >>= \t ->
                case dropAtxClosingSeq t of
                    Nothing -> return $! Header lvl (stripAsciiSpaces t)
                    Just t' -> return $! Header lvl (stripAsciiSpaces t')

-- | TODO
-- 'Nothing' indicates a failure to drop an optional closing sequence.
dropAtxClosingSeq :: Text -> Maybe Text
dropAtxClosingSeq t =
    case T.dropWhileEnd isAtxHeaderChar $
         T.dropWhileEnd isAsciiSpaceChar t of
         t' | T.null t'                            -> Nothing
            | (not . isAsciiSpaceChar . T.last) t' -> Nothing
            | otherwise                            -> Just $! T.init t'

atxHeaderLevel :: Parser Int
atxHeaderLevel =
    T.length <$> takeWhileLoHi isAtxHeaderChar 1 6

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
