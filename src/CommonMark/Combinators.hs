-- | Additional parsing combinators
module CommonMark.Combinators
    (
      (<++>)
    , countOrMore
    , upToCount
    ) where

import Control.Applicative hiding ((<|>), many)

import Text.Parsec
import Text.Parsec.Text (Parser)

-- | "Lifted" version of @(++)@.
(<++>) :: (Applicative f) => f [a] -> f [a] -> f [a]
(<++>) = liftA2 (++)

-- | @countOrMore n p@ parses @n@ or more occurrences of @p@.
countOrMore :: Int -> Parser a -> Parser [a]
countOrMore n p = count n p <++> many p

-- | @upToCount n p@ parses @n@ or less occurrences of @p@.
upToCount :: Int -> Parser a -> Parser [a]
upToCount n p = takeXWhileIsJustX <$> count n (optionMaybe p)

-- | TODO
takeXWhileIsJustX :: [Maybe a] -> [a]
takeXWhileIsJustX []            = []
takeXWhileIsJustX (Just x : xs) = x : takeXWhileIsJustX xs
