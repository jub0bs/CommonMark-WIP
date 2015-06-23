module CommonMark.Parser where

import Control.Applicative hiding ( (<|>), many )
import Data.Text                  ( Text )

import Text.Parsec
import Text.Parsec.Char (noneOf)
import Text.Parsec.Text (Parser)


import CommonMark.Types
import CommonMark.Combinators

asciiSpace :: Parser Char
asciiSpace = char ' '

-- line :: Parser [Char]
-- line = many (noneOf "\n\r")


--cmarkSource = line `sepEndBy` eol

eol :: Parser String
eol =   try (string "\r\n")
    <|> string "\r"
    <|> string "\n"
    <?> "end of line"


-- Horizontal rule
hRule :: Parser Block
hRule = Hrule
    <$ upToCount 3 asciiSpace
    <* (choice . map hRuleSequence) "*-_"
    <?> "horizontal rule"
  where
    hRuleSequence c = countOrMore 3 (char c <* many asciiSpace)
