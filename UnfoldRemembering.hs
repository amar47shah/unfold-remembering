module UnfoldRemembering (unfoldR, unfoldRLast) where

import Control.Arrow (second)
import Data.List (unfoldr)
import Data.Tuple (swap)
import Safe (lastMay)

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
