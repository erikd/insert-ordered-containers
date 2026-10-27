{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | @lens@ support for 'InsOrdHashSet'.
module Data.HashSet.InsOrd.Lens (
    hashSet,
    ) where

import Control.Lens
       (At (..), Contains (..), Index, Iso', IxValue, Ixed (..), iso)
import Data.Functor  ((<&>))
import Data.Hashable (Hashable)

import           Data.HashSet        (HashSet)
import           Data.HashSet.InsOrd (InsOrdHashSet)
import qualified Data.HashSet.InsOrd as InsOrd

type instance Index (InsOrdHashSet a) = a
type instance IxValue (InsOrdHashSet a) = ()

instance (Eq k, Hashable k) => Ixed (InsOrdHashSet k) where
    ix k f s
        | InsOrd.member k s = s <$ f ()
        | otherwise         = pure s
    {-# INLINE ix #-}

instance (Eq k, Hashable k) => At (InsOrdHashSet k) where
    at k f s = f mv <&> \r -> case r of
        Nothing -> maybe s (const (InsOrd.delete k s)) mv
        Just () -> InsOrd.insert k s
      where mv = if InsOrd.member k s then Just () else Nothing
    {-# INLINE at #-}

instance (Eq a, Hashable a) => Contains (InsOrdHashSet a) where
    contains k f s = f (InsOrd.member k s) <&> \b ->
        if b then InsOrd.insert k s else InsOrd.delete k s
    {-# INLINE contains #-}

-- | This is a slight lie, as roundtrip doesn't preserve ordering.
hashSet :: Iso' (InsOrdHashSet a) (HashSet a)
hashSet = iso InsOrd.toHashSet InsOrd.fromHashSet
