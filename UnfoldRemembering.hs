module UnfoldRemembering (unfoldR, unfoldRLast) where

import Control.Arrow (first, second)
import Control.Category ((<<<), (>>>))
import Data.List (unfoldr)
import Data.Tuple (swap)
import Safe (lastMay)
import Test.Tasty
import Test.Tasty.QuickCheck as QC

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
unfoldRLast' f x = case f x of
  (Nothing, x') -> ([], x')
  (Just y , x') -> first (y:) $ unfoldRLast' f x'

-- | Fold and Unfold as explicit duals.
foldr'   :: (Maybe (a, b) -> b           ) -> [a] -> b
unfoldr' :: (b            -> Maybe (a, b)) -> b   -> [a]

foldr'   f = f <<< fmap (second $   foldr' f) <<< headAndTailMay
unfoldr' f = f >>> fmap (second $ unfoldr' f) >>> consMay

headAndTailMay :: [a]            -> Maybe (a, [a])
consMay        :: Maybe (a, [a]) -> [a]

headAndTailMay []             = Nothing
headAndTailMay (x : xs)       = Just (x, xs)
consMay        Nothing        = []
consMay        (Just (x, xs)) = x : xs

-- Test

half :: Int -> Maybe (Int, Int)
half 0 = Nothing
half n = Just (n - m, m) where m = div n 2

add :: Maybe (Int, Int) -> Int
add Nothing       = 0
add (Just (x, y)) = x + y

main :: IO ()
main = defaultMain qcProps

qcProps :: TestTree
qcProps = testGroup "QuickCheck"
  [ QC.testProperty "foldr' add . unfoldr' half" $
      \x -> (x > 0) ==> x == (foldr' add . unfoldr' half) x
  ]
