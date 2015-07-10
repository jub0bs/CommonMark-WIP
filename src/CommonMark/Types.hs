-- Types related to the final AST (adapted from Cheapskate.Types)

module CommonMark.Types where

import Data.Sequence ( Seq )
import qualified Data.Map as M
-- import qualified Data.Map.Strict as M
import Data.Text ( Text )


-- | Root of the document's AST
-- ParseOptions are included here so that we know how a Doc was obtained.
data Doc = Doc ParseOptions Blocks
  deriving (Show)

-- | Block-level element
data Block = Hrule
           | Header !Int Inlines
           | CodeBlock !InfoString !Text
           | HtmlBlock !Text
           | Paragraph Inlines
           | Blockquote Blocks
           | List !Bool {- loose or tight -} !ListType Items
           deriving (Show)

type InfoString = Text

-- the Item type is for making list items more explicit in the AST
type Items = Seq Item

newtype Item = Item Blocks
  deriving (Show)

type Blocks = Seq Block

data ListType = Bullet  !BulletType
              | Ordered !NumDelim !StartNum
              deriving (Show, Eq)

data BulletType = Hyphen
                | PlusSign
                | Asterisk
                deriving (Show, Eq)

data NumDelim = FullStop
              | RightParen
              deriving (Show, Eq)

type StartNum = Int


-- | Inlines elements

data Inline = Inline Text -- FIXME
  deriving (Show)

type Inlines = Seq Inline


-- | Map of link references

type RefMap = M.Map Text                -- label
                    (Text, Maybe Text)  -- (destination, optional title)


-- | Parsing options
-- (Note: rendering options should go with the renderer, not here)

data ParseOptions = ParseOptions
    { option1 :: !Bool -- placeholder
    , option2 :: !Bool -- placeholder
    } deriving (Show)
