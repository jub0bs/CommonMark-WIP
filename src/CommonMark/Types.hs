module CommonMark.Types where

import Data.Text ( Text )

-- Types
data Doc = Doc [Block]
  deriving (Show)

data Block
    = Hrule
    | Header {-# UNPACK #-} !Int !Text
    -- TODO
  deriving (Show)

data ListType
    = Bullet  BulletType
    | Ordered NumDelim {-# UNPACK #-} !Int
  deriving (Show, Eq)

data BulletType = Hyphen | PlusSign | Asterisk
  deriving (Show, Eq)

data NumDelim = FullStop | RightParen
  deriving (Show, Eq)
