{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | @optics@ support for 'InsOrdHashSet'.
module Data.HashSet.InsOrd.Optics (
    hashSet,
    ) where

import Data.Functor  ((<&>))
import Data.Hashable (Hashable)
import Optics.Core
       (At (..), Contains (..), Index, Iso', IxValue, Ixed (..), atraversalVL,
       iso, lensVL)

import           Data.HashSet        (HashSet)
import           Data.HashSet.InsOrd (InsOrdHashSet)
import qualified Data.HashSet.InsOrd as InsOrd

type instance Index (InsOrdHashSet a) = a
type instance IxValue (InsOrdHashSet a) = ()

instance (Eq k, Hashable k) => Ixed (InsOrdHashSet k) where
    ix k = atraversalVL $ \point f s ->
        if InsOrd.member k s then s <$ f () else point s
    {-# INLINE ix #-}

instance (Eq k, Hashable k) => At (InsOrdHashSet k) where
    at k = lensVL $ \f s ->
        let mv = if InsOrd.member k s then Just () else Nothing
        in f mv <&> \r -> case r of
            Nothing -> maybe s (const (InsOrd.delete k s)) mv
            Just () -> InsOrd.insert k s
    {-# INLINE at #-}

instance (Eq a, Hashable a) => Contains (InsOrdHashSet a) where
    contains k = lensVL $ \f s -> f (InsOrd.member k s) <&> \b ->
        if b then InsOrd.insert k s else InsOrd.delete k s
    {-# INLINE contains #-}

-- | This is a slight lie, as roundtrip doesn't preserve ordering.
hashSet :: Iso' (InsOrdHashSet a) (HashSet a)
hashSet = iso InsOrd.toHashSet InsOrd.fromHashSet
