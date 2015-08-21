-- | This module provides functions for processing 'Text' values.
module CommonMark.Util.Text
    (
    -- * Tab and NUL replacement
      replaceNullChars
    , detab
    -- * Whitespace
    , stripAsciiSpaces
    , stripAsciiSpacesAndNewlines
    , collapseWhitespace
    -- * ATX headers
    , stripATXSuffix
    ) where

import Data.Text ( Text )
import qualified Data.Text as T

import Data.CharSet ( CharSet )
import qualified Data.CharSet                  as CS
import qualified Data.CharSet.Unicode.Category as CS ( punctuation, space )

import CommonMark.Util.Char ( isWhitespace, replacementChar )

-- | Replace null characters (U+0000) with the replacement character (U+FFFD).
replaceNullChars :: Text -> Text
replaceNullChars = T.map replaceNUL
  where
    replaceNUL c
        |  c == '\NUL' = replacementChar
        | otherwise    = c

-- | Converts tabs to spaces using a 4-space tab stop. Intended to operate on
-- a single line of input.
detab :: Text -> Text
detab = T.concat . pad . T.split (== '\t')
  where
    pad []               = []
    pad [t]              = [t]
    pad (t : ts@(_ : _)) = T.justifyLeft n ' ' t : pad ts
      where
          tl   = T.length t
          n    = tl - (tl `rem` tabw) + tabw
          tabw = 4

-- | Remove leading and trailing ASCII spaces from a string.
stripAsciiSpaces :: Text -> Text
stripAsciiSpaces = T.dropAround (== ' ')

-- | Remove leading and trailing ASCII spaces and newlines from a string.
stripAsciiSpacesAndNewlines :: Text -> Text
stripAsciiSpacesAndNewlines = T.dropAround (\c -> c == ' ' || c == '\n')

-- | Collapse each whitespace span to a single ASCII space.
collapseWhitespace :: Text -> Text
collapseWhitespace = T.intercalate (T.singleton ' ') . codeSpanWords

-- | Breaks a 'Text' up into a list of words, delimited by 'Char's
-- representing whitespace (as defined by the CommonMark spec).
-- Similar but different from 'Data.Text.words'.
codeSpanWords :: Text -> [Text]
codeSpanWords = go
  where
    go t | T.null word = []
         | otherwise   = word : go rest
      where (word, rest) = T.break isWhitespace $
                           T.dropWhile isWhitespace t
{-# INLINE codeSpanWords #-}

-- | @stripATXSuffix t@ strips @t@ from its ATX-header suffix (if any).
stripATXSuffix :: Text -> Text
stripATXSuffix t
    | T.null t'              = t
    | (' ' /=) . T.last $ t' = t
    | otherwise              = T.init t'
  where
    t' = T.dropWhileEnd (== '#') .
         T.dropWhileEnd (== ' ') $ t
