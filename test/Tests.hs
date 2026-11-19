module Main (main) where

import Prelude        ()
import Prelude.Compat

import Data.Data               (Data, cast, gmapT)
import Data.Foldable.WithIndex (ifoldr, itoList)
import Data.Hashable           (Hashable (..))
import Data.List               (nub)
import Data.Maybe              (fromMaybe)
import Data.Traversable        (foldMapDefault)
import Data.Word               (Word8)
import Text.Read               (readMaybe)

import qualified Data.Aeson                 as Aeson
import qualified Data.HashMap.Strict        as HashMap
import qualified Data.HashMap.Strict.InsOrd as InsOrd
import qualified Data.HashSet.InsOrd        as InsOrdSet
import qualified Data.List                  as List

import Test.QuickCheck.Function
import Test.Tasty
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain $ testGroup "tests"
    [ testGroup "Properties" $
        [ testProperty "fromList keeps keys in order of first occurrence" $ fromListKeys
        , testProperty "fromList keeps the last value of each key" $ fromListValues
        , testProperty "fromList agrees with repeated insert" $ fromListInsert
        , testProperty "toList distributes over mappend" $ toListMappendDistribute
        , testProperty "behaves like HashMap" $ operationModel
        , testProperty "valid" $ validProperty
        , testProperty "Hashable agree" $ hashableProperty
        , testProperty "aeson roundtrip" $ aesonRoundtrip
        , testProperty "show . read = id" showReadRoundtrip
        , testProperty "Eq respects ordering" eqRespectsOrdering
        , testProperty "Ord compares entries in insertion order" ordMap
        , testProperty "InsOrdHashSet Ord compares elements in insertion order" ordSet
        , testProperty "traverseWithKey effects in insertion order" traverseWithKeyOrder
        ]
    , testGroup "Regressions"
        [ testProperty "issue 10: union overflow" $ issue10
        , testProperty "issue 10: unionWithKey overflow" $ issue10WithKey
        , testProperty "issue 12 Foldable" $ issue12a
        , testProperty "issue 12 Traversable" $ issue12b
        , testProperty "issue 12 FoldableWithIndex ifoldr" $ issue12c
        , testProperty "issue 12 FoldableWithIndex itoList" $ issue12d
        , testProperty "issue 19 Data InsOrdHashMap" $ issue19Map
        , testProperty "issue 19 Data InsOrdHashSet" $ issue19Set
        , testProperty "issue 19 Data InsOrdHashMap identity" $ issue19MapIdentity
        , testProperty "issue 19 Data InsOrdHashSet identity" $ issue19SetIdentity
        , testProperty "issue 27 InsOrdHashSet Eq/Hashable ignore indices" $ issue27
        , testProperty "InsOrdHashSet union is valid and ordered" $ setUnion
        ]
    ]

fromListKeys :: [(Word8, Int)] -> Property
fromListKeys l = InsOrd.keys (InsOrd.fromList l) === nub (map fst l)

fromListValues :: [(Word8, Int)] -> Property
fromListValues l = InsOrd.toHashMap (InsOrd.fromList l) === HashMap.fromList l

fromListInsert :: [(Word8, Int)] -> Property
fromListInsert l = InsOrd.fromList l === List.foldl' (\m (k, v) -> InsOrd.insert k v m) InsOrd.empty l

toListMappendDistribute :: [(Int, Int)] -> [(Int, Int)] -> Property
toListMappendDistribute a b = rhs === lhs
  where
    a' = InsOrd.fromList a
    b' = foldr InsOrd.delete (InsOrd.fromList b) (InsOrd.keys a')
    rhs = InsOrd.toList (a' <> b')
    lhs = InsOrd.toList a' <> InsOrd.toList b'

-------------------------------------------------------------------------------
-- Model
-------------------------------------------------------------------------------

data Operation k v
    = FromList [(k, v)]
    | Empty
    | Singleton k v
    | Insert k v (Operation k v)
    | Delete k (Operation k v)
    | Union (Operation k v) (Operation k v)
    | Difference (Operation k v) (Operation k v)
    | Intersection (Operation k v) (Operation k v)
    | Filter (Fun v Bool) (Operation k v)
    deriving (Show)

instance (Arbitrary k, Arbitrary v, Function v, CoArbitrary v) => Arbitrary (Operation k v) where
    arbitrary = sized a
      where
          term =
              [ FromList <$> arbitrary
              , pure Empty
              , Singleton <$> arbitrary <*> arbitrary
              ]
          a 0 = oneof term
          a n = oneof $ term ++
              [ Insert <$> arbitrary <*> arbitrary <*> aMinus1
              , Delete <$> arbitrary <*> aMinus1
              , Union <$> aDiv2 <*> aDiv2
              , Difference <$> aDiv2 <*> aDiv2
              , Intersection <$> aDiv2 <*> aDiv2
              , Filter <$> arbitrary <*> aMinus1
              ]
            where
              aMinus1 = a (n - 1)
              aDiv2   = a (n `div` 2)

evalOpInsOrd
    :: (Eq k, Hashable k)
    => Operation k v -> InsOrd.InsOrdHashMap k v
evalOpInsOrd op = case op of
    FromList l         -> InsOrd.fromList l
    Empty              -> InsOrd.empty
    Singleton k v      -> InsOrd.singleton k v
    Insert k v a       -> InsOrd.insert k v (evalOpInsOrd a)
    Delete k a         -> InsOrd.delete k (evalOpInsOrd a)
    Union a b          -> InsOrd.union (evalOpInsOrd a) (evalOpInsOrd b)
    Difference a b     -> InsOrd.difference (evalOpInsOrd a) (evalOpInsOrd b)
    Intersection a b   -> InsOrd.intersection (evalOpInsOrd a) (evalOpInsOrd b)
    Filter (Fun _ f) a -> InsOrd.filter f (evalOpInsOrd a)

evalOpHashMap
    :: (Eq k, Hashable k)
    => Operation k v-> HashMap.HashMap k v
evalOpHashMap op = case op of
    FromList l         -> HashMap.fromList l
    Empty              -> HashMap.empty
    Singleton k v      -> HashMap.singleton k v
    Insert k v a       -> HashMap.insert k v (evalOpHashMap a)
    Delete k a         -> HashMap.delete k (evalOpHashMap a)
    Union a b          -> HashMap.union (evalOpHashMap a) (evalOpHashMap b)
    Difference a b     -> HashMap.difference (evalOpHashMap a) (evalOpHashMap b)
    Intersection a b   -> HashMap.intersection (evalOpHashMap a) (evalOpHashMap b)
    Filter (Fun _ f) a -> HashMap.filter f (evalOpHashMap a)

operationModel :: Operation Word8 Int -> Property
operationModel op = rhs === lhs
  where
    iom = evalOpInsOrd op
    lhs = InsOrd.toHashMap iom
    rhs = evalOpHashMap op

validProperty :: Operation Word8 Int -> Property
validProperty op = property $ InsOrd.valid iom
  where
    iom = evalOpInsOrd op

hashableProperty :: Operation Word8 Int -> Int -> Property
hashableProperty op salt = rhs === lhs
  where
    iom = evalOpInsOrd op
    lhs = hashWithSalt salt $ iom
    rhs = hashWithSalt salt $ evalOpHashMap op

aesonRoundtrip :: Operation Int Int -> Property
aesonRoundtrip op = rhs === lhs
  where
    iom = evalOpInsOrd op
    rhs = Right iom 
    lhs = Aeson.eitherDecode $ Aeson.encode iom

showReadRoundtrip :: Operation Word8 Int -> Property
showReadRoundtrip op = rhs === lhs
  where
    iom = evalOpInsOrd op
    rhs = Just iom
    lhs = readMaybe $ show iom


eqRespectsOrdering :: Operation Int Int -> Property
eqRespectsOrdering op = iom == iomRev ==> InsOrd.toList iom === InsOrd.toList iomRev
  where
    iom = evalOpInsOrd op
    iomRev = InsOrd.fromList . reverse . InsOrd.toList $ iom

ordMap :: Operation Word8 Word8 -> Operation Word8 Word8 -> Property
ordMap opA opB =
    compare a b === compare (InsOrd.toList a) (InsOrd.toList b)
    .&&. (compare a b == EQ) === (a == b)
  where
    a = evalOpInsOrd opA
    b = evalOpInsOrd opB

ordSet :: [Word8] -> [Word8] -> Property
ordSet xs ys =
    compare a b === compare (InsOrdSet.toList a) (InsOrdSet.toList b)
    .&&. (compare a b == EQ) === (a == b)
  where
    a = InsOrdSet.fromList xs
    b = InsOrdSet.fromList ys

traverseWithKeyOrder :: Operation Word8 Int -> Property
traverseWithKeyOrder op =
    -- effects (which just accumulate key/value pairs here) ran in order:
    effects === InsOrd.toList iom
    .&&. InsOrd.toList iom' === [ (k, v + 1) | (k, v) <- InsOrd.toList iom ]
    .&&. property (InsOrd.valid iom')
  where
    iom = evalOpInsOrd op
    (effects, iom') = InsOrd.traverseWithKey (\k v -> ([(k, v)], v + 1)) iom

-------------------------------------------------------------------------------
-- Regressions
-------------------------------------------------------------------------------

-- 'unionWithKey' did not get the index repacking added to 'unionWith' for
-- issue 10, so repeated unions overflowed the insertion stamps.
issue10WithKey :: Property
issue10WithKey = InsOrd.elems p === "wold!" .&&. property (InsOrd.valid p)
  where
    m, n, p :: InsOrd.InsOrdHashMap Char Char
    m = InsOrd.fromList  (zip "hello" "world")
    n = iterate (\x -> InsOrd.unionWithKey (\_ a _ -> a) x x) m !! 64
    p = InsOrd.insert '!' '!' n

issue12a :: Property
issue12a = foldr (:) [] m === "wold"
  where
    m :: InsOrd.InsOrdHashMap Char Char
    m = InsOrd.fromList  (zip "hello" "world")

issue12b :: Property
issue12b = foldMapDefault (:[]) m === "wold"
  where
    m :: InsOrd.InsOrdHashMap Char Char
    m = InsOrd.fromList  (zip "hello" "world")

issue12c :: Property
issue12c = ifoldr (\_ v acc -> v : acc) [] m === "wold"
  where
    m :: InsOrd.InsOrdHashMap Char Char
    m = InsOrd.fromList  (zip "hello" "world")

issue12d :: Property
issue12d = itoList m === zip "helo" "wold"
  where
    m :: InsOrd.InsOrdHashMap Char Char
    m = InsOrd.fromList  (zip "hello" "world")


-- Apply @f@ to every 'Int' inside a value, bottom-up, like syb's
-- @everywhere (mkT f)@.
mapInts :: Data a => (Int -> Int) -> a -> a
mapInts f = go
  where
    go :: Data b => b -> b
    go x = atInt (gmapT go x)

    atInt :: Data b => b -> b
    atInt x = fromMaybe x (cast . f =<< cast x)

issue19Map :: [(Int, Int)] -> Property
issue19Map l = InsOrd.toList m' === expected .&&. property (InsOrd.valid m')
  where
    m = InsOrd.fromList l
    m' = mapInts negate m
    expected = [ (negate k, negate v) | (k, v) <- InsOrd.toList m ]

issue19Set :: [Int] -> Property
issue19Set l =
    InsOrdSet.toList s' === expected .&&. property (InsOrdSet.valid s')
  where
    s = InsOrdSet.fromList l
    s' = mapInts negate s
    expected = map negate (InsOrdSet.toList s)

-- An identity generic transformation must yield an equal container, even
-- when duplicate insertions or deletions have left gaps in the internal
-- insertion stamps.
issue19MapIdentity :: [(Int, Int)] -> [Int] -> Property
issue19MapIdentity l ds =
    m' === m .&&. hash m' === hash m .&&. property (InsOrd.valid m')
  where
    m = List.foldl' (flip InsOrd.delete) (InsOrd.fromList (l ++ l)) ds
    m' = gmapT id m

issue19SetIdentity :: [Int] -> [Int] -> Property
issue19SetIdentity l ds =
    s' === s .&&. hash s' === hash s .&&. property (InsOrdSet.valid s')
  where
    s = List.foldl' (flip InsOrdSet.delete) (InsOrdSet.fromList (l ++ l)) ds
    s' = gmapT id s

-- Sets with the same elements in the same order must be equal and hash
-- alike, regardless of how they were built; equality is order sensitive.
issue27 :: Int -> Property
issue27 salt =
    a === b
    .&&. a =/= InsOrdSet.fromList [3, 1]
    .&&. hashWithSalt salt a === hashWithSalt salt b
    .&&. hashWithSalt salt a === hashWithSalt salt (InsOrdSet.toHashSet a)
  where
    a, b :: InsOrdSet.InsOrdHashSet Int
    a = InsOrdSet.delete 2 (InsOrdSet.fromList [1, 2, 3])
    b = InsOrdSet.fromList [1, 3]

-- 'InsOrdHashSet.union' used to give the right-hand set's last element a
-- stamp equal to the counter, so the result was not 'valid' and a later
-- 'insert' could collide with it.
setUnion :: [Int] -> [Int] -> Int -> Property
setUnion xs ys z =
    InsOrdSet.toList u === expected
    .&&. property (InsOrdSet.valid u)
    .&&. property (InsOrdSet.valid u')
    .&&. InsOrdSet.toList u' === expected'
  where
    a = InsOrdSet.fromList xs
    b = InsOrdSet.fromList ys
    u = InsOrdSet.union a b
    u' = InsOrdSet.insert z u
    expected =
        InsOrdSet.toList a
        ++ [ y | y <- InsOrdSet.toList b, not (InsOrdSet.member y a) ]
    expected' = List.filter (/= z) expected ++ [z]

issue10 :: Property
issue10 = InsOrd.elems p === "wold!" .&&. property (InsOrd.valid p)
  where
    m, n, p :: InsOrd.InsOrdHashMap Char Char
    m = InsOrd.fromList  (zip "hello" "world")
    n = iterate (\x -> InsOrd.union x x) m !! 64
    p = InsOrd.insert '!' '!' n
