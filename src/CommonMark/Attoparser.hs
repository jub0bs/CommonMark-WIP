{-# LANGUAGE OverloadedStrings #-}

module CommonMark.Attoparser
    ( endOfLine
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


-- Blocks

-- | Parse a horizontal rule. Intended to operate on a single line of input.
hRule :: Parser Block
hRule = Hrule
    <$  skipNonIndentSpace
    <*  (choice . map hRuleSequence) hRuleChars
    <*  endOfInput
    <?> "horizontal rule"
  where
    hRuleSequence c = countOrMore 3 (char c <* skipAsciiSpaces)
    hRuleChars      = "*-_"


-- ATX headers

isAtxHeaderChar :: Char -> Bool
isAtxHeaderChar = (== '#')

-- | Parse an ATX header. Intended to operate on a single line of input.
atxHeader :: Parser Block
atxHeader = do
    skipNonIndentSpace
    lvl       <- atxHeaderLevel
    maybeChar <- peekChar
    case maybeChar of
        Nothing                   -> return $! Header lvl T.empty
        Just c | isNonSpaceChar c -> failure
        _                         -> do
            t <- stripAsciiSpaces . removeATXSuffix <$> takeText
            return $! Header lvl t

-- | Parse the opening sequence of #s of an ATX header and return the header
-- level.
atxHeaderLevel :: Parser Int
atxHeaderLevel =
    T.length <$> takeWhileLoHi isAtxHeaderChar 1 6

-- | Remove an optional ATX-header suffix from a 'Text' value.
removeATXSuffix :: Text -> Text
removeATXSuffix t =
    let t' = T.dropWhileEnd isAtxHeaderChar  .
             T.dropWhileEnd isAsciiSpaceChar $ t
    in
        if T.null t' || (not . isAsciiSpaceChar . T.last) t'
        then t
        else T.init t'


--- setext headers (see section 4.3 of the CommonMark specs)

-- | Parse a setext-header underline and return the header level. Intended
-- to operate on a single line of input.
setextHeaderUnderLine :: Parser Int
setextHeaderUnderLine =
        skipAsciiSpaces0to3
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
    skipAsciiSpaces0to3
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
