{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | @optics@ support for 'InsOrdHashMap'.
module Data.HashMap.Strict.InsOrd.Optics (
    hashMap,
    unorderedTraversal,
    ) where

import Data.Functor  ((<&>))
import Data.Hashable (Hashable)
import Optics.Core
       (At (..), Index, Iso, IxValue, Ixed (..), Traversal, atraversalVL, iso,
       lensVL, traversed, (%))

import           Data.HashMap.Strict        (HashMap)
import           Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import qualified Data.HashMap.Strict.InsOrd as InsOrd

type instance Index (InsOrdHashMap k v) = k
type instance IxValue (InsOrdHashMap k v) = v

instance (Eq k, Hashable k) => Ixed (InsOrdHashMap k v) where
    ix k = atraversalVL $ \point f m -> case InsOrd.lookup k m of
        Just v  -> f v <&> \v' -> InsOrd.insert k v' m
        Nothing -> point m
    {-# INLINE ix #-}

instance (Eq k, Hashable k) => At (InsOrdHashMap k v) where
    at k = lensVL $ \f m ->
        let mv = InsOrd.lookup k m
        in f mv <&> \r -> case r of
            Nothing -> maybe m (const (InsOrd.delete k m)) mv
            Just v' -> InsOrd.insert k v' m
    {-# INLINE at #-}

-- | This is a slight lie, as roundtrip doesn't preserve ordering.
hashMap :: Iso (InsOrdHashMap k a) (InsOrdHashMap k b) (HashMap k a) (HashMap k b)
hashMap = iso InsOrd.toHashMap InsOrd.fromHashMap

unorderedTraversal :: Traversal (InsOrdHashMap k a) (InsOrdHashMap k b) a b
unorderedTraversal = hashMap % traversed
