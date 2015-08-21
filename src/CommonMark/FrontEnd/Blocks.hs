{-# LANGUAGE OverloadedStrings #-}

-- | This module provides parsers for determining the block structure
-- of a CommonMark document (phase 1).
-- See <http://spec.commonmark.org/0.21/#container-blocks-and-leaf-blocks>
-- for more details.
--
-- Note: all parsers, unless specified otherwise,  are intended to operate on
-- a single line of input.
module CommonMark.FrontEnd.Blocks
    (
    -- * Leaf parsers
      hRule
    , atxHeader
    , setextHeaderUnderLine
    , openingCodeFence
    , closingCodeFence

    -- * Node parsers
    ) where

import Control.Applicative ( (<|>), liftA2 )
import Data.Text ( Text )
import qualified Data.Text as T
import Prelude hiding ( lines )

import Data.Attoparsec.Text ( Parser, (<?>) )
import qualified Data.Attoparsec.Text as A

import CommonMark.Util.Char
import CommonMark.Util.Parsing
import CommonMark.Util.Text
import CommonMark.Types

-- | Parses the lines of input.
lines :: Parser [Text]
lines = line `A.sepBy` endOfLine

-- | Parses a line of input.
line :: Parser Text
line = A.takeTill A.isEndOfLine

-- | Skips a CommonMark line-ending sequence.
endOfLine :: Parser ()
endOfLine = discard $ A.choice
    [ A.string "\r\n"
    , A.string "\r"
    , A.string "\n"
    ]

-- | A node (container) of the intermediate representation.
-- FIXME
-- data Node =

-- | A leaf of the intermediate syntax tree.
data Leaf = TextLine !Text
          | BlankLine !Text
          | ATXHeader {-# UNPACK #-} !Int !Text
          | SetextHeader {-# UNPACK #-} !Int !Text
          | Rule
          deriving (Show)

-- | Skips one ASCII space character.
skipAsciiSpace :: Parser ()
skipAsciiSpace = discard $ A.char ' '

-- | Skips zero or more ASCII space characters.
skipAsciiSpaces :: Parser ()
skipAsciiSpaces = discard $ A.takeWhile (== ' ')

-- | Skips one or more ASCII space characters.
skipAsciiSpaces1 :: Parser ()
skipAsciiSpaces1 = discard $ A.takeWhile1 (== ' ')

-- | Parses zero to three ASCII spaces characters, and returns the number
-- of such characters it has parsed.
asciiSpaces0to3 :: Parser Int
asciiSpaces0to3 = T.length <$> takeWhileHi (== ' ') 3

-- | Skips zero to three ASCII space characters.
skipAsciiSpaces0to3 :: Parser ()
skipAsciiSpaces0to3 = discard asciiSpaces0to3

-- | Skips non-indent spaces.
skipNonIndentSpace :: Parser ()
skipNonIndentSpace = skipAsciiSpaces0to3

-- | Skips zero or more whitespace characters.
whitespace :: Parser ()
whitespace = discard $ A.takeWhile isWhitespace

-- | Parses a horizontal rule.
hRule :: Parser Leaf
hRule = Rule <$  skipNonIndentSpace
             <*  (A.choice . map hRuleSequence) hRuleChars
             <*  A.endOfInput
             <?> "horizontal rule"
  where
    hRuleSequence c = countOrMore 3 (A.char c <* skipAsciiSpaces)
    hRuleChars      = "*-_"

-- | Parses an ATX header.
atxHeader :: Parser Leaf
atxHeader = ATXHeader <$> atxHeaderLevel
                      <*> atxHeaderRawContents
                      <?> "ATX header"

-- | Parses the opening sequence of #s of an ATX header and return the header
-- level.
atxHeaderLevel :: Parser Int
atxHeaderLevel = T.length <$> atxHeaderPrefix
                          <?> "ATX-header level"

-- | Parses the opening sequence of hash signs of an ATX header.
atxHeaderPrefix :: Parser Text
atxHeaderPrefix =
       skipNonIndentSpace
    *> takeWhileLoHi (== '#') 1 6
   <*  notFollowedBy (not . isWhitespace)
   <?> "ATX-header prefix"

-- | Parse the raw contents of an ATX header.
atxHeaderRawContents :: Parser Text
atxHeaderRawContents =
    stripAsciiSpaces . stripATXSuffix <$> A.takeText
                                      <?> "ATX-header raw contents"

-- | Parses a setext-header underline, and returns the header level.
setextHeaderUnderLine :: Parser Int
setextHeaderUnderLine =
        skipNonIndentSpace
     *> setextHeaderLevel
    <*  skipAsciiSpaces
    <*  A.endOfInput
    <?> "setext-header underline"

-- | Parses the sequence of = or - characters of a setext-header underline
-- and return the header level.
setextHeaderLevel :: Parser Int
setextHeaderLevel =     1 <$ A.takeWhile1 (== '=')
                    <|> 2 <$ A.takeWhile1 (== '-')
                    <?> "setext-header level"

-- | A fence type (backtick or tilde).
newtype FenceType = FenceType Char
    deriving (Show, Eq)

-- | Parses a code-fence sequence.
codeFence :: Parser (FenceType, Int)
codeFence = go '`' <|> go '~'
  where
    go c = do
        n <- T.length <$> takeWhileLo (== c) 3
        return (FenceType c, n)

-- | Parses the info string of a code fence.
infoString :: Parser InfoString
infoString = stripAsciiSpaces <$> A.takeWhile (/= '`')
                              <*  A.endOfInput

-- FIXME: return indentation
--               FenceType
--               number of chars in code fence
--               infostring
openingCodeFence :: Parser Text
openingCodeFence = do
    lvl <- asciiSpaces0to3
    (fencetype, n) <- codeFence
    t <- infoString
    return t

-- FIXME
closingCodeFence :: FenceType -> Int -> Parser ()
closingCodeFence fencetype n = do
    skipNonIndentSpace
    (fencetype', n') <- codeFence
    if fencetype' /= fencetype || n' < n
    then failure
    else do
        skipAsciiSpaces
        A.endOfInput
        return ()

-- | Parses a 'TextLine' or 'BlankLine'.
textOrBlankLine :: Parser Leaf
textOrBlankLine = textOrBlank <$> A.takeText
  where
    textOrBlank t
        | T.all (\c -> c == ' ' || c == '\t') t = BlankLine t
        | otherwise                             = TextLine t

-- | Parses a blockquote marker.
blockQuoteMarker :: Parser ()
blockQuoteMarker =    skipNonIndentSpace
                   *> A.char '>'
                   *> optional skipAsciiSpace
