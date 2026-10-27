module Main (main) where

import Data.Function (on)
import Data.List     (sort)
import Optics.Core

import qualified Data.HashMap.Strict.InsOrd as InsOrd
import qualified Data.HashSet.InsOrd        as InsOrdSet

import Data.HashMap.Strict.InsOrd.Optics
import Data.HashSet.InsOrd.Optics

import Test.Tasty
import Test.Tasty.QuickCheck

type M = InsOrd.InsOrdHashMap Int Int
type S = InsOrdSet.InsOrdHashSet Int

main :: IO ()
main = defaultMain $ testGroup "insert-ordered-containers-optics"
    [ testGroup "InsOrdHashMap"
        [ testProperty "view at = lookup" $ \kvs k ->
            let m = InsOrd.fromList kvs :: M
            in view (at k) m === InsOrd.lookup k m
        , testProperty "set at (Just v) = insert" $ \kvs k v ->
            let m = InsOrd.fromList kvs :: M
            in (m & at k ?~ v) === InsOrd.insert k v m
        , testProperty "set at Nothing = delete" $ \kvs k ->
            let m = InsOrd.fromList kvs :: M
            in (m & at k .~ Nothing) === InsOrd.delete k m
        , testProperty "preview ix = lookup" $ \kvs k ->
            let m = InsOrd.fromList kvs :: M
            in preview (ix k) m === InsOrd.lookup k m
        , testProperty "over ix = adjust" $ \kvs k f ->
            let m = InsOrd.fromList kvs :: M
            in (m & ix k %~ applyFun f) === InsOrd.adjust (applyFun f) k m
        , testProperty "view hashMap = toHashMap" $ \kvs ->
            let m = InsOrd.fromList kvs :: M
            in view hashMap m === InsOrd.toHashMap m
        , testProperty "unorderedTraversal visits every element" $ \kvs ->
            let m = InsOrd.fromList kvs :: M
            in ((===) `on` sort) (toListOf unorderedTraversal m) (InsOrd.elems m)
        ]
    , testGroup "InsOrdHashSet"
        [ testProperty "view contains = member" $ \ks k ->
            let s = InsOrdSet.fromList ks :: S
            in view (contains k) s === InsOrdSet.member k s
        , testProperty "set contains True = insert for absent k" $ \ks k ->
            let s = InsOrdSet.fromList ks :: S
            in not (InsOrdSet.member k s) ==>
               (s & contains k .~ True) === InsOrdSet.insert k s
        -- Disabled pending #24.
        -- , testProperty "set contains (view contains) = id" $ \ks k ->
        --     let s = InsOrdSet.fromList ks :: S
        --     in (s & contains k .~ view (contains k) s) === s
        , testProperty "set contains False = delete" $ \ks k ->
            let s = InsOrdSet.fromList ks :: S
            in (s & contains k .~ False) === InsOrdSet.delete k s
        , testProperty "view at = member" $ \ks k ->
            let s = InsOrdSet.fromList ks :: S
            in view (at k) s === (if InsOrdSet.member k s then Just () else Nothing)
        , testProperty "set at (Just ()) = insert for absent k" $ \ks k ->
            let s = InsOrdSet.fromList ks :: S
            in not (InsOrdSet.member k s) ==>
               (s & at k ?~ ()) === InsOrdSet.insert k s
        -- Disabled pending #24.
        -- , testProperty "set at (view at) = id" $ \ks k ->
        --     let s = InsOrdSet.fromList ks :: S
        --     in (s & at k .~ view (at k) s) === s
        , testProperty "set at Nothing = delete" $ \ks k ->
            let s = InsOrdSet.fromList ks :: S
            in (s & at k .~ Nothing) === InsOrdSet.delete k s
        , testProperty "has ix = member" $ \ks k ->
            let s = InsOrdSet.fromList ks :: S
            in has (ix k) s === InsOrdSet.member k s
        , testProperty "view hashSet = toHashSet" $ \ks ->
            let s = InsOrdSet.fromList ks :: S
            in view hashSet s === InsOrdSet.toHashSet s
        ]
    ]
