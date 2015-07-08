-- Types related to the final AST (adapted from Cheapskate.Types)

module CommonMark.Types where

import qualified Data.Map as M
import           Data.Text ( Text )


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
           | List !Bool {- loose or tight -} !ListType [Blocks]
           deriving (Show)

type Blocks = [] Block -- use lists for now, switch to Seq later perhaps

type InfoString = Text

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

type Inlines = [] Inline  -- use lists for now, switch to Seq later perhaps


-- | Map of link references
type LinkLabel       = Text
type LinkDestination = Text
type LinkTitle       = Text

type ReferenceMap = M.Map LinkLabel (LinkDestination, Maybe LinkTitle)


-- | Parsing options
-- (Note: rendering options should go with the renderer, not here)

data ParseOptions = ParseOptions
    { option1 :: !Bool -- placeholder
    , option2 :: !Bool -- placeholder
    } deriving (Show)
