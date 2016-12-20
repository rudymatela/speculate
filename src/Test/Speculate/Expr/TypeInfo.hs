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
typeInfo :: (Typeable a, Listable a, Show a, Eq a, Ord a)
         => a -> String -> TypeInfo1
typeInfo x n = typeInfoNames x (namesFromTemplate n)

-- this eventually will become the new "typeInfo" constructor
typeInfoN :: (Typeable a, Listable a, Show a, Eq a, Ord a)
          => a -> String -> TypeInfo
typeInfoN x n =
  [ typeInfo    x      $ n

  , typeInfo   [x]     $ n ++ "s"
  , typeInfo  [[x]]    $ n ++ "ss"
  , typeInfo [[[x]]]   $ n ++ "ss"

  , typeInfo (x,x)     $ n ++ m
  , typeInfo (x,x,x)   $ n ++ m ++ o
  , typeInfo (x,x,x,x) $ n ++ m ++ o ++ p

  , typeInfo [(x,x)]   $ n ++ m ++ "s"
  , typeInfo [(x,x,x)] $ n ++ m ++ o ++ "ss"

  , typeInfo (x,[x])   $ n ++ m ++ "s"
  , typeInfo ([x],x)   $ n ++ "s" ++ m
  , typeInfo ([x],[x]) $ n ++ "s" ++ m ++ "s"
  , typeInfo (x,(x,x)) $ n ++ m ++ o
  , typeInfo ((x,x),x) $ n ++ m ++ o

  , typeInfo (mayb x)   $ "m" ++ n ++ "s"
  , typeInfo (eith x x) $ "e" ++ n ++ o ++ "s"
  ]
  where
  m = namesFromTemplate n !! 1
  o = namesFromTemplate m !! 1
  p = namesFromTemplate o !! 1
-- NOTE: the function typeInfoN is not perfect: it won't help produce types
-- combining different sub-types, like for example: (Bool,Int).  But it is
-- way better than the original version in which I had to explictly define
-- everything.  A definitive solution is still to be thought of.

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
  [ typeInfoN (undefined :: ())       "x"
  , typeInfoN (undefined :: Bool)     "p"

  , typeInfoN (undefined :: Int)      "x"
  , typeInfoN (undefined :: Word)     "x"
  , typeInfoN (undefined :: Integer)  "x"

  , typeInfoN (undefined :: Ordering) "o"
  , typeInfoN (undefined :: Char)     "c"

  , typeInfoN (undefined :: Rational) "q"
  , typeInfoN (undefined :: Float)    "f"
  , typeInfoN (undefined :: Double)   "f"

  , typeInfoN (undefined :: Int1)     "x"
  , typeInfoN (undefined :: Int2)     "x"
  , typeInfoN (undefined :: Int3)     "x"
  , typeInfoN (undefined :: Int4)     "x"
  , typeInfoN (undefined :: Word1)    "x"
  , typeInfoN (undefined :: Word2)    "x"
  , typeInfoN (undefined :: Word3)    "x"
  , typeInfoN (undefined :: Word4)    "x"
  , typeInfoN (undefined :: Nat1)     "x"
  , typeInfoN (undefined :: Nat2)     "x"
  , typeInfoN (undefined :: Nat3)     "x"
  , typeInfoN (undefined :: Nat4)     "x"
  , typeInfoN (undefined :: Nat5)     "x"
  , typeInfoN (undefined :: Nat6)     "x"
  , typeInfoN (undefined :: Nat7)     "x"
  ]
-- WHOA!  Have I discovered a "bug" in GHC?  adding to many type compositions
-- on typeInfoN and types on basicTypeInfo makes compilation of this module
-- *really* slow: it takes a whopping 2 minutes!
-- (the above report is using -O2, I have not tested without optimizations).


defNames :: [String]
defNames = namesFromTemplate "x"
