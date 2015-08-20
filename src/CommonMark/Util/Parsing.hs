-- | This module provides some convenient parsers and parsing combinators
-- that supplement attoparsec's built-in ones.
module CommonMark.Util.Parsing
    (
    -- * Success and failure
      success
    , failure

    -- * Combinators
    , discard
    , optional
    , notFollowedBy
    , countOrMore

    -- * Predicate/count parsers
    , takeWhileLo
    , takeWhileHi
    , takeWhileLoHi
    ) where

import Control.Applicative ( Applicative, (<|>), liftA2, many )
import Data.Text ( Text )
import qualified Data.Text as T
import Prelude hiding ( takeWhile )

import Data.Attoparsec.Text ( Parser, count, peekChar, scan, takeWhile )

-- | @success@ is a parser that always succeeds.
success :: Parser ()
success = return ()

-- | @failure@ is a parser that always fails.
failure :: Parser a
failure = fail "no parse"

-- | @discard p@ applies parser @p@ and discards the latter's result.
discard :: Parser a -> Parser ()
discard p = () <$ p

-- | @optional p@ tries to apply parser @p@ and discard the latter's result,
-- or simply succeeds. Does not fail.
optional :: Parser a -> Parser ()
optional p = discard p <|> success

-- | The parser @notFollowedBy p@ succeeds if there is no input left or if
-- the next character does not satisfy predicate @p@. Consumes no input.
notFollowedBy :: (Char -> Bool) -> Parser ()
notFollowedBy p = do
    maybeChar <- peekChar
    case maybeChar of
        Nothing -> success
        Just c | p c       -> failure
               | otherwise -> success

-- | @countOrMore n p@ applies action @p@ repeatedly, @n@ or more times,
-- and returns a list of the results.
-- Generalizes 'Data.Attoparsec.Combinator.many1'.
countOrMore :: Int -> Parser a -> Parser [a]
countOrMore n p = count n p <++> many p

-- | Plain old '(++)' lifted to applicative functors.
infixr 5 <++>
(<++>) :: (Applicative f) => f [a] -> f [a] -> f [a]
(<++>) = liftA2 (++)

-- | @takeWhileLo f lo@ consumes @lo@ characters or more as long as
-- predicate @f@ returns 'True', and returns the consumed input.
--
-- Requires the predicate to succeed on at least @lo@ characters of input.
-- Fails if the predicate never returns 'True' or if the end of the input
-- has been reached.
takeWhileLo :: (Char -> Bool) -> Int -> Parser Text
takeWhileLo f lo =
    takeWhile f >>= \t ->
    case T.compareLength t lo of
        LT -> failure
        EQ -> return t
        GT -> return t

-- | @takeWhileHi f hi@ consumes up to @hi@ characters as long as predicate
-- @f@ returns 'True', and returns the consumed input.
--
-- Does not fail. Returns an empty string if the predicate returns 'False' on
-- the first character of input.
takeWhileHi :: (Char -> Bool) -> Int -> Parser Text
takeWhileHi f hi =
    scan 0 $ \n c -> if n < hi && f c
                     then Just $! n + 1
                     else Nothing

-- | @takeWhileLoHi f lo hi@ consumes from @lo@ to @hi@ characters as long as
-- predicate @f@ returns 'True', and returns the consumed input.
--
-- Requires the predicate to succeed on at least @lo@ characters of input.
-- Fails if the predicate never returns 'True' or if the end of the input
-- has been reached.
takeWhileLoHi :: (Char -> Bool) -> Int -> Int -> Parser Text
takeWhileLoHi f lo hi =
    takeWhileHi f hi >>= \t ->
    case T.compareLength t lo of
        LT -> failure
        EQ -> return t
        GT -> return t
