{-# LANGUAGE OverloadedStrings #-}

-- | Note: all parsers, aside from @lines@, are intended to operate on
-- on a single line of input.

module CommonMark.Parser
    ( commonmark
    , endOfLine
    , line
    , hRule
    , atxHeaderLevel
    , atxHeader
    , setextHeaderUnderLine
    , openingCodeFence
    , closingCodeFence
    , blockQuoteMarker
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
commonmark = map detab . processLines


-- | Types for intermediate representation

data Leaf = TextLine !Text
          | BlankLine !Text
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


-- | Skip /one/ ASCII space character.
skipAsciiSpace :: Parser ()
skipAsciiSpace = discard $ asciiSpace

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
skipAsciiSpaces0to3 = discard asciiSpaces0to3

skipNonIndentSpace :: Parser ()
skipNonIndentSpace = skipAsciiSpaces0to3

-- | Skip /zero/ or more whitespace characters.
whitespace :: Parser ()
whitespace = discard $ takeWhile isWhiteSpaceChar

-- | Skip a CommonMark line ending.
endOfLine :: Parser ()
endOfLine =
        discard (string "\r\n")
    <|> discard (char '\r')
    <|> discard (char '\n')

-- | TODO
line :: Parser Text
line = takeTill isEndOfLineChar

-- |
lines :: Parser [Text]
lines = line `sepBy` endOfLine


-- Leaf parsers

-- 4.1 - Horizontal rules

-- | Parse a horizontal rule.
hRule :: Parser Leaf
hRule = Rule
    <$  skipNonIndentSpace
    <*  (choice . map hRuleSequence) hRuleChars
    <*  endOfInput
    <?> "horizontal rule"
  where
    hRuleSequence c = countOrMore 3 (char c <* skipAsciiSpaces)
    hRuleChars      = "*-_"


-- 4.2 - ATX headers

-- | Parse an ATX header.
atxHeader :: Parser Leaf
atxHeader =
        ATXHeader
    <$> atxHeaderLevel
    <*> atxHeaderRawContents
    <?> "ATX header"

-- | Parse the opening sequence of #s of an ATX header and return the header
-- level.
atxHeaderLevel :: Parser Int
atxHeaderLevel =
        T.length
    <$> atxHeaderPrefix
    <?> "ATX-header level"

-- | Parse the opening sequence of #s of an ATX header.
atxHeaderPrefix :: Parser Text
atxHeaderPrefix =
       skipNonIndentSpace
    *> takeWhileLoHi isATXHeaderChar 1 6
   <*  notFollowedBy isNonSpaceChar
   <?> "ATX-header prefix"

-- | Parse the raw contents of an ATX header.
atxHeaderRawContents :: Parser Text
atxHeaderRawContents =
        stripAsciiSpaces . stripATXSuffix
    <$> takeText
    <?> "ATX-header raw contents"


--- 4.3 - Setext headers

-- | Parse a setext-header underline and return the header level.
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
    <?> "setext-header level"


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
infoString :: Parser Text
infoString =
        stripAsciiSpaces
    <$> takeWhile (not . isBacktick)
    <*  endOfInput

-- | Parse an opening code fence and return TODO
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

-- lump those two together for efficiency
textOrBlankLine :: Parser Leaf
textOrBlankLine = textOrBlank <$> takeText
  where
    textOrBlank t
        | T.all (\c -> isAsciiSpaceChar c || isTab c) t = BlankLine t
        | otherwise                                     = TextLine t


blockQuoteMarker :: Parser ()
blockQuoteMarker =
    skipNonIndentSpace
    *> char '>'
    *> option () skipAsciiSpace


--- inlines

hardLineBreak :: Parser ()
hardLineBreak = ()
    <$ char '\\'
    <* endOfInput
