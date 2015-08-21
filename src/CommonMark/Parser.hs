module CommonMark.Parser where

import Data.Text ( Text )

import Data.Default ( Default(..) )

import CommonMark.Parser.Blocks
import CommonMark.Parser.Inlines
import CommonMark.Types

-- | Parses a CommonMark document.
parseCommonMark :: ParsingOptions -> Text -> Doc
parseCommonMark = undefined

-- | Parsing options
data ParsingOptions = ParsingOptions
    { option1 :: !Bool -- placeholder
    , option2 :: !Bool -- placeholder
    } deriving (Show)

instance Default ParsingOptions where
    def = ParsingOptions True True
