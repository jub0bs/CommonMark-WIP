-- | Additional parsing combinators
module CommonMark.Combinators
    ( failure
    , countOrMore
    -- , countOrFewer
    , takeWhileLo
    , takeWhileHi
    , takeWhileLoHi
    ) where

import Prelude hiding ( takeWhile )
import Control.Applicative  hiding ( (<|>) )
import Data.Text ( Text )
import qualified Data.Text as T
import Data.Attoparsec.Text

-- import Text.Parsec
-- import Text.Parsec.Text (Parser)

-- | "Lifted" version of @(++)@.
(<++>) :: (Applicative f) => f [a] -> f [a] -> f [a]
(<++>) = liftA2 (++)

-- | @failure@ is a scanner that always fails.
failure :: Parser a
failure = mempty

-- | @countOrMore n p@ applies action @p@ repeatedly, @n@ or more times,
-- and returns a list of the results.
-- (Generalizes 'Data.Attoparsec.Combinator.many1'.)
countOrMore :: Int -> Parser a -> Parser [a]
countOrMore n p = count n p <++> many p

--  FIXME
-- -- | @countOrFewer n p@ applies action @p@ repeatedly, zero to @n@ times,
-- -- and returns a list of the results.
-- countOrFewer :: Int -> Parser a -> Parser [a]
-- countOrFewer n p = takeXWhileIsJustX <$> count n (optionMaybe p)
--   where
--     -- takeXWhileIsJustX :: [Maybe a] -> [a]
--     takeXWhileIsJustX []            = []
--     takeXWhileIsJustX (Just x : xs) = x : takeXWhileIsJustX xs

-- | @takeWhileLo f lo@ consumes at least @lo@ characters as long as
-- predicate @f@ returns 'True', and returns the consumed input.
--
-- This parser requires the predicate to succeed on at least @lo@
-- characters of input: it will fail if the predicate never returns
-- 'True' or if there is no input left.
takeWhileLo :: (Char -> Bool) -> Int -> Parser Text
takeWhileLo f lo =
    takeWhile f >>= \t ->
    case T.compareLength t lo of
        LT -> failure
        _  -> return t

-- | @takeWhileHi f hi@ consumes up to @hi@ characters as long as
-- predicate @f@ returns 'True', and returns the consumed input.
--
-- This parser does not fail. It will return an empty string if the predicate
-- returns 'False' on the first character of input.
--
-- (adapted from @Cheapskate.Combinators.upToCountChars@)
--
-- FIXME: returns a continuation if fed hi characters that satisfy the
-- predicate
takeWhileHi :: (Char -> Bool) -> Int -> Parser Text
takeWhileHi f hi =
    scan 0 $ \n c -> if n < hi && f c
                     then Just $! n + 1
                     else Nothing

-- | @takeWhileLoHi f lo hi@ consumes from @lo@ to @hi@ characters as long as
-- predicate @f@ returns 'True', and returns the consumed input.
--
-- This parser requires the predicate to succeed on at least @lo@
-- characters of input: it will fail if the predicate never returns
-- 'True' or if there is no input left.
takeWhileLoHi :: (Char -> Bool) -> Int -> Int -> Parser Text
takeWhileLoHi f lo hi =
    takeWhileHi f hi >>= \t ->
    case T.compareLength t lo of
        LT -> failure
        _  -> return t
