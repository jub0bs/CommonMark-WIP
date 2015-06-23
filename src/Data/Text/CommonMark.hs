{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}

module Data.Text.CommonMark where

import Control.Applicative hiding ((<|>), optional, many)
import Control.Monad              (forever)
import Data.Maybe                 (catMaybes)
import Text.Parsec
import Text.Parsec.Char           (noneOf)

-- Debugging
parseFromFile p fname = do
    input <- readFile fname
    return $! (runParser p () fname input)

parseToEOF p = parse (p <* eof) ""

-- Types
data Doc = Doc [Block]
  deriving (Show)

data Block
    = Hrule
    -- | Header {-# UNPACK #-} !Int Inlines
    -- TODO
  deriving (Show)

data ListType
    = Bullet  !BulletType
    | Ordered !NumDelim {-# UNPACK #-} !Int
  deriving (Show, Eq)

data BulletType = Hyphen | PlusSign | Asterisk
  deriving (Show, Eq)

data NumDelim = FullStop | RightParen
  deriving (Show, Eq)

-- additional combinators
(<++>) = liftA2 (++)
countOrMore n p = count n p <++> many p
upToCount n p = takeWhileIsJust <$> count n (optionMaybe p)

takeWhileIsJust :: [Maybe a] -> [a]
takeWhileIsJust (Just x : xs) = x : takeWhileIsJust xs
takeWhileIsJust _             = []
-- TODO: inline or not?

-- Parser
asciiSpace = char ' '

line = many (noneOf "\n\r")

cmarkSource = line `sepEndBy` eol

eol =   try (string "\r\n")
    <|> string "\r"
    <|> string "\n"
    <?> "end of line"


-- Horizontal rule
hRule = Hrule
    <$ upToCount 3 asciiSpace
    <* (choice . map hRuleSequence) "*-_"
    <?> "horizontal rule"
  where
    hRuleSequence c = countOrMore 3 (char c <* many asciiSpace)


-- ATX header

