{-# LANGUAGE OverloadedStrings #-}

-- | This module implements parsing and rendering of CommonMark documents.
--
-- CommonMark is a standard, unambiguous syntax specification for Markdown.
-- See <http://commonmark.org> for more details.
--
--   * The implementation is intended to ultimately become fully compliant
--     with the Commonmark Spec. At this stage of development, only some
--     syntactic elements are parsed.
--
--   * The current export list of this module is tentative; the module
--     currently exports some entities only for testing purposes.
--
module CommonMark
    (
    -- * Testing
      commonmarkTest

    -- * Parsers

    -- ** Blocks
    , hRule
    , atxHeader
    , setextHeaderUnderLine

    -- ** Inlines
    , escapedChar
    , entity
    , namedEntity
    , numericEntity
    , codeSpan
    , autolink
    , absoluteURI
    , emailAddress
    )
    where

import Data.Text ( Text )

import Data.Attoparsec.Text ( Parser )
import qualified Data.Attoparsec.Text as A

import CommonMark.Parser.Blocks
import CommonMark.Parser.Inlines
-- import CommonMark.Renderer.Html
import CommonMark.Types
import CommonMark.Util.Parsing

-- | Temporary function for testing elementary parsers.
commonmarkTest :: Show a => Parser a -> Text -> IO ()
commonmarkTest p s = do
    case A.parseOnly p s of
      Left e  -> putStrLn $ "No parse: " ++ e
      Right x -> putStrLn $ "Parsed: " ++ show x
