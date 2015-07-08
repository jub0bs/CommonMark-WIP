{-# LANGUAGE OverloadedStrings #-}

module CommonMark.Parser
    ( commonmark
    , endOfLine
    , line
    , hRule
    , atxHeaderLevel
    , atxHeader
    , removeATXSuffix
    , setextHeaderUnderLine
    , openingCodeFence
    , closingCodeFence
    , infoString
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

commonmark :: Text -> [Text] -- FIXME
commonmark = processLines


-- | Types for intermediate representation

data Leaf = TextLine !Text
          | BlanLine !Text
          | ATXHeader !Int !Text
          | SetextHeader !Int !Text
          | Rule
          deriving (Show)

-- temporary
processLines :: Text -> [Text]
processLines t = ts
  where Right ts = parseOnly lines t -- (lines never fails)


-- | @discard p@ applies action @p@ but discards its result.
discard :: Parser a -> Parser ()
discard p = () <$ p


-- Whitespace / scanner
--
-- | Parse an ASCII space character.
asciiSpace :: Parser Char
asciiSpace = satisfy isAsciiSpaceChar

-- | Skip /zero/ or more ASCII space characters.
skipAsciiSpaces :: Parser ()
skipAsciiSpaces = discard $ takeWhile isAsciiSpaceChar

-- | Skip /one/ or more ASCII space characters.
skipAsciiSpaces1 :: Parser ()
skipAsciiSpaces1 = discard $ takeWhile1 isAsciiSpaceChar

asciiSpaces0to3 :: Parser Int
asciiSpaces0to3 = T.length <$> takeWhileHi isAsciiSpaceChar 3


-- | Skip between /zero/ and /three/ ASCII space characters.
skipAsciiSpaces0to3 :: Parser ()
skipAsciiSpaces0to3 = discard $ asciiSpaces0to3

skipNonIndentSpace :: Parser ()
skipNonIndentSpace = skipAsciiSpaces0to3

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


-- Leaf parsers

-- | Parse a horizontal rule. Intended to operate on a single line of input.
hRule :: Parser Leaf
hRule = Rule
    <$  skipNonIndentSpace
    <*  (choice . map hRuleSequence) hRuleChars
    <*  endOfInput
    <?> "horizontal rule"
  where
    hRuleSequence c = countOrMore 3 (char c <* skipAsciiSpaces)
    hRuleChars      = "*-_"


-- ATX headers

-- | Parse an ATX header. Intended to operate on a single line of input.
atxHeader :: Parser Leaf
atxHeader = do
    skipNonIndentSpace
    lvl       <- atxHeaderLevel
    maybeChar <- peekChar
    case maybeChar of
        Nothing                   -> return $! ATXHeader lvl T.empty
        Just c | isNonSpaceChar c -> failure
        _                         -> do
            t <- stripAsciiSpaces . removeATXSuffix <$> takeText
            return $! ATXHeader lvl t

-- | Parse the opening sequence of #s of an ATX header and return the header
-- level.
atxHeaderLevel :: Parser Int
atxHeaderLevel =
    T.length <$> takeWhileLoHi isAtxHeaderChar 1 6


-- | Remove an optional ATX-header suffix from a 'Text' value.
-- TODO: move to Util?
removeATXSuffix :: Text -> Text
removeATXSuffix t =
    if T.null t' || (not . isAsciiSpaceChar . T.last) t'
        then t
        else T.init t'
  where
    t' = T.dropWhileEnd isAtxHeaderChar  .
         T.dropWhileEnd isAsciiSpaceChar $ t

isAtxHeaderChar :: Char -> Bool
isAtxHeaderChar c = c == '#'

--- setext headers (see section 4.3 of the CommonMark specs)

-- | Parse a setext-header underline and return the header level. Intended
-- to operate on a single line of input.
setextHeaderUnderLine :: Parser Int
setextHeaderUnderLine =
        skipNonIndentSpace
     *> setextHeaderLevel
    <*  skipAsciiSpaces
    <*  endOfInput
    <?> "setext-header underline"

-- | Parse the sequence of = or - characters of a setext-header underline
-- and return the header level.
setextHeaderLevel :: Parser Int
setextHeaderLevel =
        1 <$ takeWhile1 (== '=')
    <|> 2 <$ takeWhile1 (== '-')


-- Fenced code blocks

data FenceType = FenceType Char
    deriving (Show, Eq)

-- | Parse a code-fence sequence (three or more backticks, or three of more
-- tildes)
codeFence :: Parser (FenceType, Int)
codeFence = go '`' <|> go '~'
  where
    go c = do
        n <- T.length <$> takeWhileLo (== c) 3
        return (FenceType c, n)

-- | Parse the info string of a code fence.
-- Intended to operate on a single line of input.
infoString :: Parser Text
infoString =
        stripAsciiSpaces
    <$> takeWhile (not . isBacktick)
    <*  endOfInput

-- | Parse an opening code fence and return TODO
-- Intended to operate on a single line of input.
-- TODO: return indentation
--              FenceType
--              number of chars in code fence
--              infostring
openingCodeFence :: Parser Text
openingCodeFence = do
    lvl <- asciiSpaces0to3
    (fencetype, n) <- codeFence
    t <- infoString
    return t

-- | Parse an closing code fence and return TODO
-- Intended to operate on a single line of input.
-- TODO: return FenceType
--              number of chars in code fence
closingCodeFence :: FenceType -> Int -> Parser ()
closingCodeFence fencetype n = do
    skipNonIndentSpace
    (fencetype', n') <- codeFence
    if fencetype' /= fencetype || n' < n
    then failure
    else do
        skipAsciiSpaces
        endOfInput
        return ()


--- inlines

hardLineBreak :: Parser ()
hardLineBreak = ()
    <$ char '\\'
    <* endOfInput
