module CommonMark where

import Control.Applicative ((<|>))
import Data.Char (isSpace)
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Trifecta.Parser
-- import Data.Text.CommonMark.Types

-- Types
data Doc = Doc [Block]
  deriving (Show)

data Block
    = Hrule
    -- | Header {-# UNPACK #-} !Int Inlines
    -- TODO
  deriving (Show)

data ListType
    = Bullet  {-# UNPACK #-} !BulletType
    | Ordered {-# UNPACK #-} !NumDelim
              {-# UNPACK #-} !Int
  deriving (Show, Eq)

data BulletType = Hyphen | PlusSign | Asterisk
  deriving (Show, Eq)

data NumDelim = FullStop | RightParen
  deriving (Show, Eq)


-- Debugging
parseToEOF p = parseTest $ p <* eof


-- Char

-- asciiPunctuationChar :: CharParsing m => m Char
-- asciiPunctuationChar = oneOf "!\"# $% &'()*+,-./:;<=>?@[\\]^_`{|}~"

asciiSpace :: CharParsing m => m Char
asciiSpace = char ' '


-- Parser
eol :: CharParsing m => m ()
eol = (() <$ newline) <|> (carriageReturn *> skipOptional newline)
  where
    newline        = char '\n'
    carriageReturn = char '\r'


-- Horizontal rule
hRule :: CharParsing m => m Block
hRule = Hrule
    <$ threeOrFewer asciiSpace
    <* (choice . map hRuleSequence) "*-_"
  where
    threeOrFewer p  = count 3 (optional p)
    threeOrMore  p  = count 3 p <* skipMany p
    hRuleSequence c = threeOrMore (char c <* many asciiSpace)
