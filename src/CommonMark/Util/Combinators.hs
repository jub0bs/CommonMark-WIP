-- | Some convenient parsing combinators

module CommonMark.Util.Combinators
    ( discard
    , failure
    , success
    , notFollowedBy
    , countOrMore
    , takeWhileLo
    , takeWhileHi
    , takeWhileLoHi
    ) where

import Prelude hiding ( takeWhile )
import Control.Applicative hiding ( (<|>) )
import Data.Text ( Text )
import qualified Data.Text as T
import Data.Attoparsec.Text

-- | Plain old '(++)' lifted to applicative functors.
infixr 5 <++>
(<++>) :: (Applicative f) => f [a] -> f [a] -> f [a]
(<++>) = liftA2 (++)

-- | @discard p@ applies action @p@ but discards its result.
discard :: Parser a -> Parser ()
discard p = () <$ p

-- | @failure@ is a parser that always fails.
failure :: Parser a
failure = mempty

-- | @success@ is a parser that always succeeds.
success :: Parser ()
success = return ()

-- | @notFollowedBy p@ succeeds if there is no input left or if the next
-- character does not satisfy predicate @p@.
-- Consumes no input.
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

-- | @takeWhileLo f lo@ consumes @lo@ characters or more as long as
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
        EQ -> return t
        GT -> return t

-- | @takeWhileHi f hi@ consumes up to @hi@ characters as long as
-- predicate @f@ returns 'True', and returns the consumed input.
--
-- This parser does not fail. It will return an empty string if the predicate
-- returns 'False' on the first character of input.

-- (Adapted from @Cheapskate.Combinators.upToCountChars@)
--
-- FIXME: returns a continuation if fed 'hi' characters that satisfy the
-- predicate... why?
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
