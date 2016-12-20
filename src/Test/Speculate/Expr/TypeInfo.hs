module Test.Speculate.Expr.TypeInfo
  ( TypeInfo
  , TypeInfo1 (..)
  , TypeRep

  -- * Smart constructors
  , typeInfo
  , typeInfoNames

  -- * Queries on TypeInfo1 lists
  , isComparable
  , findInfo
  , existsInfo
  , names
  , equalityE
  , compareE
  , lessEqE
  , lessE
  , tiersE

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

import Data.Maybe (isJust)
import Data.List (find,(\\))


-- | Type information needed to Speculate expressions (single type).
data TypeInfo1 = TypeInfo1
  { typerep1   :: TypeRep
  , equalityE1 :: Expr     -- equality function expression
  , compareE1  :: Expr     -- comparison function expression
  , lessEqE1   :: Expr
  , lessE1     :: Expr
  , tiersE1    :: [[Expr]] -- tiers of expressions
  , names1     :: [String] -- infinite list of template names for that type
  }

-- TODO: allow partially specifying TypeInfo
-- data TypeInfo1 = Eq TypeRep Expr
--                | Compare TypeRep Expr
--                ... ... ...
--                | Tiers TypeRep [[Expr]]
--                | Names TypeRep [String]

-- | Type information needed to Speculate expressions.
type TypeInfo = [TypeInfo1]

-- | Usage: @typeInfo (undefined :: Type) "x"@
typeInfo1 :: (Typeable a, Listable a, Show a, Eq a, Ord a)
          => a -> String -> TypeInfo1
typeInfo1 x n = typeInfoNames x (namesFromTemplate n)

-- this eventually will become the new "typeInfo" constructor
typeInfo :: (Typeable a, Listable a, Show a, Eq a, Ord a)
         => a -> String -> TypeInfo
typeInfo x n =
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
  , typeInfo1 ([x],[x]) $ n ++ "s" ++ m ++ "s"
--, typeInfo1 (x,(x,x)) $ n ++ m ++ o
--, typeInfo1 ((x,x),x) $ n ++ m ++ o

  , typeInfo1 (mayb x)   $ "m" ++ n ++ "1"
  , typeInfo1 (eith x x) $ "e" ++ n ++ o ++ "1"
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
              => a -> [String] -> TypeInfo1
typeInfoNames x ns = TypeInfo1
  { typerep1   = typeOf x
  , equalityE1 = constant "=="        $ (errorToFalse .: (==)) -:> x
  , compareE1  = constant "`compare`" $ compare                -:> x
  , lessEqE1   = constant "<="        $ (errorToFalse .: (<=)) -:> x
  , lessE1     = constant "<"         $ (errorToFalse .: (<))  -:> x
  , tiersE1    = mapT showConstant (tiers `asTypeOf` [[x]])
  , names1     = ns
  }
  where
  (.:) = (.) . (.)

findInfo :: TypeRep -> TypeInfo -> Maybe TypeInfo1
findInfo t = find ((== t) . typerep1)

existsInfo :: TypeInfo -> TypeRep -> Bool
existsInfo ti = isJust . (`findInfo` ti)


isComparable :: TypeInfo -> Expr -> Bool
isComparable ti = isJust . (`findInfo` ti) . typ


names :: TypeRep -> TypeInfo -> [String]
names t ti =
  case findInfo t ti of
    Nothing -> defNames
    Just ti -> names1 ti

tiersE :: TypeRep -> TypeInfo -> [[Expr]]
tiersE t ti =
  case findInfo t ti of
    Nothing -> error $ "could not find type information for " ++ show t
    Just ti -> tiersE1 ti

equalityE :: TypeInfo -> TypeRep -> Maybe Expr
equalityE ti = fmap equalityE1 . (`findInfo` ti)

compareE :: TypeInfo -> TypeRep -> Maybe Expr
compareE ti = fmap compareE1 . (`findInfo` ti)

lessE :: TypeInfo -> TypeRep -> Maybe Expr
lessE ti = fmap lessE1 . (`findInfo` ti)

lessEqE :: TypeInfo -> TypeRep -> Maybe Expr
lessEqE ti = fmap lessEqE1 . (`findInfo` ti)

-- TODO: include *ALL* prelude types on basicTypeInfo
basicTypeInfo :: TypeInfo
basicTypeInfo = concat
  [ typeInfo (undefined :: ())       "x"
  , typeInfo (undefined :: Bool)     "p"

  , typeInfo (undefined :: Int)      "x"
  , typeInfo (undefined :: Word)     "x"
  , typeInfo (undefined :: Integer)  "x"

  , typeInfo (undefined :: Ordering) "o"
  , typeInfo (undefined :: Char)     "c"

  , typeInfo (undefined :: Rational) "q"
  , typeInfo (undefined :: Float)    "f"
  , typeInfo (undefined :: Double)   "f"

-- TODO: uncomment the following and investigate why compilation takes so long
--, typeInfo (undefined :: Int1)     "x"
  , typeInfo (undefined :: Int2)     "x"
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
