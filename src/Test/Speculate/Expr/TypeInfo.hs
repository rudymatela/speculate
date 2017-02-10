module Test.Speculate.Expr.TypeInfo
  ( TypeInfo
  , TypeInfo1 (..)
  , TypeRep

  -- * Smart constructors
  , typeInfo
  , typeInfoNames

  -- * Queries on TypeInfo1 lists
  , findInfo
  , names
  , eqE,      isEq
  , leE, ltE, isOrd
  ,           isEqOrd
  , tiersE,   isListable

  -- * Type info for standard Haskell types
  , basicTypeInfo

  -- * Does not belong here?
  , defNames

  , boolTy
  , mkEqnTy
  )
where

import Test.Speculate.Expr.Core
import Test.Speculate.Expr.Match
import Test.Speculate.Utils
import Test.LeanCheck
import Test.LeanCheck.Utils hiding (comparison)
import Test.LeanCheck.Error (errorToFalse)
import Data.Dynamic

import Data.Maybe (isJust,fromMaybe,listToMaybe,catMaybes)
import Data.List (find,(\\))


-- | Type information needed to Speculate expressions (single type / single class).
data TypeInfo1 = Eq TypeRep Expr
               | Ord TypeRep Expr Expr
               | Listable TypeRep [[Expr]]
               | Names TypeRep [String]
-- TODO: rename TypeInfo1 -> Instance
-- TODO: rename TypeInfo -> Instances

-- | Type information needed to Speculate expressions.
type TypeInfo = [TypeInfo1]

-- | Usage: @typeInfo (undefined :: Type) "x"@
typeInfo1 :: (Typeable a, Listable a, Show a, Eq a, Ord a)
          => a -> String -> TypeInfo
typeInfo1 x n = typeInfoNames x (namesFromTemplate n)

-- this eventually will become the new "typeInfo" constructor
typeInfo :: (Typeable a, Listable a, Show a, Eq a, Ord a)
         => a -> String -> TypeInfo
typeInfo x n = concat
  [ typeInfo1    x      $ n

  , typeInfo1   [x]     $ n ++ "s"
  , typeInfo1  [[x]]    $ n ++ "ss"
--, typeInfo1 [[[x]]]   $ n ++ "ss"

  , typeInfo1 (x,x)     $ n ++ m
  , typeInfo1 (x,x,x)   $ n ++ m ++ o
--, typeInfo1 (x,x,x,x) $ n ++ m ++ o ++ p

  , typeInfo1 [(x,x)]   $ n ++ m ++ "s"
--, typeInfo1 [(x,x,x)] $ n ++ m ++ o ++ "ss"

--, typeInfo1 (x,[x])   $ n ++ m ++ "s"
--, typeInfo1 ([x],x)   $ n ++ "s" ++ m
--, typeInfo1 ([x],[x]) $ n ++ "s" ++ m ++ "s"
--, typeInfo1 (x,(x,x)) $ n ++ m ++ o
--, typeInfo1 ((x,x),x) $ n ++ m ++ o

  , typeInfo1 (mayb x)   $ "m" ++ n ++ "1"
--, typeInfo1 (eith x x) $ "e" ++ n ++ o ++ "1"
  ]
  where
  m = namesFromTemplate n !! 1
  o = namesFromTemplate m !! 1
  p = namesFromTemplate o !! 1
-- NOTE: the function typeInfoN is not perfect: it won't help produce types
-- combining different sub-types, like for example: (Bool,Int).  But it is
-- way better than the original version in which I had to explictly define
-- everything.  A definitive solution is still to be thought of.
-- NOTE: see related TODO on the definition of basicTypeInfo

-- | Usage: @typeInfoNames (undefined :: Type) ["x","y","z","w",...]@
--
-- You are probably better off using 'typeInfo'
typeInfoNames :: (Typeable a, Listable a, Show a, Eq a, Ord a)
              => a -> [String] -> TypeInfo
typeInfoNames x ns =
  [ Eq       (typeOf x) $ constant "==" $ (errorToFalse .: (==)) -:> x
  , Ord      (typeOf x) (constant "<=" $ (errorToFalse .: (<=)) -:> x)
                        (constant "<"  $ (errorToFalse .: (<))  -:> x)
  , Listable (typeOf x) $ mapT showConstant (tiers `asTypeOf` [[x]])
  , Names    (typeOf x) $ ns
  ]
  where
  (.:) = (.) . (.)

-- TODO: make types consistent!  add isOrdE and isEqE?
isOrd :: TypeInfo -> Expr -> Bool
isOrd ti = isJust . ltE ti . typ

isEq :: TypeInfo -> Expr -> Bool
isEq ti = isJust . eqE ti . typ

isEqOrd :: TypeInfo -> Expr -> Bool
isEqOrd ti e = isOrd ti e && isEq ti e

isListable :: TypeInfo -> TypeRep -> Bool
isListable ti t = isJust $ findInfo m ti
  where
  m (Listable t' ts) | t' == t = Just ts
  m _                          = Nothing

-- TODO: implement above using something similar to the following
-- isComparable ti = isJust . (`findInfo` ti) . typ

findInfo :: (TypeInfo1 -> Maybe a) -> TypeInfo -> Maybe a
findInfo may = listToMaybe . catMaybes . map may

findInfoOr :: a -> (TypeInfo1 -> Maybe a) -> TypeInfo -> a
findInfoOr def may = fromMaybe def . findInfo may

names :: TypeInfo -> TypeRep -> [String]
names ti t = findInfoOr defNames m ti
  where
  m (Names t' ns) | t == t' = Just ns
  m _                       = Nothing

tiersE :: TypeInfo -> TypeRep -> [[Expr]]
tiersE ti t = findInfoOr (error $ "could not find Listable " ++ show t) m ti
  where
  m (Listable t' ts) | t == t' = Just ts
  m _                          = Nothing

eqE :: TypeInfo -> TypeRep -> Maybe Expr
eqE ti t = findInfo m ti
  where
  m (Eq t' eq) | t == t' = Just eq
  m _                    = Nothing

ltE :: TypeInfo -> TypeRep -> Maybe Expr
ltE ti t = findInfo m ti
  where
  m (Ord t' _ lt) | t == t' = Just lt
  m _                       = Nothing

leE :: TypeInfo -> TypeRep -> Maybe Expr
leE ti t = findInfo m ti
  where
  m (Ord t' le _) | t == t' = Just le
  m _                       = Nothing

-- TODO: include *ALL* prelude types on basicTypeInfo
basicTypeInfo :: TypeInfo
basicTypeInfo = concat
  [ typeInfo1 (undefined :: ()) "x"
  , typeInfo1 (undefined :: [()]) "xs"

  , typeInfo (undefined :: Bool)     "p"

  , typeInfo (undefined :: Int)      "x"
--, typeInfo (undefined :: Word)     "x"
  , typeInfo (undefined :: Integer)  "x"

  , typeInfo (undefined :: Ordering) "o"
  , typeInfo (undefined :: Char)     "c"

  , typeInfo (undefined :: Rational) "q"
  , typeInfo (undefined :: Float)    "f"
  , typeInfo (undefined :: Double)   "f"

-- TODO: uncomment the following and investigate why compilation takes so long
--, typeInfo (undefined :: Int1)     "x"
--, typeInfo (undefined :: Int2)     "x"
--, typeInfo (undefined :: Int3)     "x"
--, typeInfo (undefined :: Int4)     "x"
--, typeInfo (undefined :: Word1)    "x"
  , typeInfo (undefined :: Word2)    "x"
--, typeInfo (undefined :: Word3)    "x"
--, typeInfo (undefined :: Word4)    "x"
--, typeInfo (undefined :: Nat1)     "x"
--, typeInfo (undefined :: Nat2)     "x"
--, typeInfo (undefined :: Nat3)     "x"
--, typeInfo (undefined :: Nat4)     "x"
--, typeInfo (undefined :: Nat5)     "x"
--, typeInfo (undefined :: Nat6)     "x"
--, typeInfo (undefined :: Nat7)     "x"
  ]
-- WHOA!  Have I discovered a "bug" in GHC?  adding to many type compositions
-- on typeInfoN and types on basicTypeInfo makes compilation of this module
-- *really* slow: it takes a whopping 2 minutes!
-- (the above report is using -O2, I have not tested without optimizations).


defNames :: [String]
defNames = namesFromTemplate "x"
