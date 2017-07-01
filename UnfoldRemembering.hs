module UnfoldRemembering (unfoldR, unfoldRLast) where

import Control.Arrow (second)
import Control.Category ((<<<), (>>>))
import Data.List (unfoldr)
import Data.Tuple (swap)
import Safe (headMay, lastMay, tailMay)

-- | Unfold and return a list of result-intermediate tuples.
-- Compare to unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldR :: (b -> Maybe (a, b)) -> b -> [(a, b)]
unfoldR f =
  uncurry (zipWith (,)) . unfoldR' f

-- | Unfold and return a list of results with the last intermediate.
unfoldRLast :: (b -> Maybe (a, b)) -> b -> ([a], b)
unfoldRLast f =
  second . lastOr <*> unfoldR' f

lastOr :: a -> [a] -> a
lastOr x = maybe x id . lastMay

unfoldR' :: (b -> Maybe (a, b)) -> b -> ([a], [b])
unfoldR' f =
  swap . sequenceA . unfoldr (remembering f)

remembering :: Functor m => (b0 -> m (a, b)) -> b0 -> m (([b], a), b)
remembering f b0 = (\(a, b) -> (([b], a), b)) <$> f b0

-- | Simpler version of unfoldRLast, with different unfolding function type.
unfoldRLast' :: (b -> (Maybe a, b)) -> b -> ([a], b)
unfoldRLast' f x =
  case f x of
    (Nothing, x') -> ([], x')
    (Just y , x') -> let (ys, x'') = unfoldRLast' f x' in (y:ys, x'')

-- | Fold and Unfold as explicit duals.
foldr' :: (Maybe (a, b) -> b) -> [a] -> b
foldr' f = f <<< fmap (\(x, xs) -> (x, foldr' f xs)) <<< headAndTailMay

unfoldr' :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr' f = f >>> fmap (\(x, xs) -> (x, unfoldr' f xs)) >>> consMay

headAndTailMay :: [a] -> Maybe (a, [a])
headAndTailMay []     = Nothing
headAndTailMay (x:xs) = Just (x, xs)

consMay :: Maybe (a, [a]) -> [a]
consMay Nothing = []
consMay (Just (x, xs)) = x:xs
