module Data.HashMap.InsOrd.Internal where

import Prelude hiding (filter, foldr, lookup, map, null)

import Data.List (sortBy)
import Data.Ord  (comparing)

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.IntMap.Strict  as IntMap

-------------------------------------------------------------------------------
-- Ordered traversal
-------------------------------------------------------------------------------

-- | Traverse a 'HashMap', sequencing effects in ascending order of the given
-- index rather than in the map's internal order. The result has the same
-- keys as the input. The index must be distinct for every entry.
--
-- O(n log n): sort the entries by index and traverse that list, then look
-- each result up by its index. (This replaces an insertion-sorting free
-- applicative, which was O(n^2).)
traverseWithKeyOrderedBy
    :: Applicative f
    => (a -> Int) -> (k -> a -> f b) -> HashMap k a -> f (HashMap k b)
traverseWithKeyOrderedBy ix f m =
    refill <$> traverse g (sortBy (comparing fst) [(ix a, (k, a)) | (k, a) <- HashMap.toList m])
  where
    g (i, (k, a)) = (,) i <$> f k a

    -- Results come back in ascending index order. We reuse m's structure rather
    -- than rebuilding with 'fromList', since that would need 'Hashable k', which
    -- the 'Traversable' instance can't require.
    refill rs =
        let im = IntMap.fromDistinctAscList rs
        in HashMap.map (\a -> im IntMap.! ix a) m
