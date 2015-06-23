{-# LANGUAGE OverloadedStrings #-}

module CommonMark.Debugging
    (
      parseFromFile
    , parseToEOF      -- for checking that the entire input has been parsed
    ) where

import           Data.Text           ( Text)
import qualified Data.Text.IO as TIO

import           Text.Parsec
import           Text.Parsec.Text (Parser)

-- Debugging
parseFromFile :: Parser a -> FilePath -> IO (Either ParseError a)
parseFromFile p fname = do
    input <- TIO.readFile fname
    return $! (runParser p () fname input)

parseToEOF :: Parser a -> Text -> Either ParseError a
parseToEOF p = parse (p <* eof) ""
