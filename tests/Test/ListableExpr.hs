module Test.ListableExpr
  (
  -- * The Expr type
    Expr

  -- * Expressions of a type
  , IntE (..)
  , BoolE (..)
  , IntsE (..)
  , CharE (..)

  , IntE0 (..)
  , IntEV (..)
  , BoolE0 (..)
  , BoolEV (..)
  , IntsE0 (..)
  , IntsEV (..)
  , CharE0 (..)
  , CharEV (..)

  -- ** Functional values
  , IntToIntE (..)
  , IntToIntToIntE (..)
  , BoolToBoolE (..)
  , BoolToBoolToBoolE (..)

  , SameTypeE (..)
  , unSameTypeE
  , SameTypedPairsE (..)

  -- * Terminal expressions
  , E0 (..)
  , EV (..)

  -- * Ill typed expressions
  , Ill (..)
  )
where

-- TODO: StringE

import Test.LeanCheck
import Test.LeanCheck.Function.ShowFunction
import Data.Haexpress.Fixtures
import Data.Function (on)

-- | Terminal constants.
newtype E0  =  E0 { unE0 :: Expr }

-- | Variables.
newtype EV  =  EV { unEV :: Expr }

-- | Expression of 'Int' type.
newtype IntE  =  IntE { unIntE :: Expr }

-- | Constant terminal value of 'Int' type.
newtype IntE0  =  IntE0 { unIntE0 :: Expr }

-- | Varialbe of 'Int' type.
newtype IntEV  =  IntEV { unIntEV :: Expr }

-- | Functions from Int to Int
newtype IntToIntE  =  IntToIntE { unIntToIntE :: Expr }
newtype IntToIntToIntE  =  IntToIntToIntE { unIntToIntToIntE :: Expr }

-- | Expression of 'Bool' type.
newtype BoolE  =  BoolE { unBoolE :: Expr }

-- | Constant terminal value of 'Bool' type.
newtype BoolE0  =  BoolE0 { unBoolE0 :: Expr }

-- | Varialbe of 'Bool' type.
newtype BoolEV  =  BoolEV { unBoolEV :: Expr }

-- | Functions from Bool to Bool
newtype BoolToBoolE  =  BoolToBoolE { unBoolToBoolE :: Expr }
newtype BoolToBoolToBoolE  =  BoolToBoolToBoolE { unBoolToBoolToBoolE :: Expr }

newtype CharE  =  CharE { unCharE :: Expr }

newtype CharE0  =  CharE0 { unCharE0 :: Expr }

newtype CharEV  =  CharEV { unCharEV :: Expr }

data SameTypeE  =  SameTypeE Expr Expr

unSameTypeE :: SameTypeE -> (Expr,Expr)
unSameTypeE (SameTypeE e1 e2)  =  (e1,e2)

data SameTypedPairsE  =  SameTypedPairsE { unSameTypedPairsE :: [(Expr,Expr)] }

-- | Ill typed expressions.
newtype Ill  =  Ill { unIll :: Expr }


instance Show E0  where  show (E0 e) = show e
instance Show EV  where  show (EV e) = show e

instance Show IntE  where  show (IntE e) = show e

instance Show IntE0  where  show (IntE0 e) = show e
instance Show IntEV  where  show (IntEV e) = show e

instance Show IntToIntE  where  show (IntToIntE e) = show e
instance Show IntToIntToIntE  where  show (IntToIntToIntE e) = show e

instance Show BoolE  where  show (BoolE e) = show e

instance Show BoolE0  where  show (BoolE0 e) = show e
instance Show BoolEV  where  show (BoolEV e) = show e

instance Show BoolToBoolE  where  show (BoolToBoolE e) = show e
instance Show BoolToBoolToBoolE  where  show (BoolToBoolToBoolE e) = show e

instance Show IntsE  where  show (IntsE e) = show e

instance Show IntsE0  where  show (IntsE0 e) = show e
instance Show IntsEV  where  show (IntsEV e) = show e

instance Show CharE  where  show (CharE e) = show e

instance Show CharE0  where  show (CharE0 e) = show e
instance Show CharEV  where  show (CharEV e) = show e

instance Show SameTypeE  where  show (SameTypeE e1 e2) = show (e1,e2)

instance Show SameTypedPairsE  where  show (SameTypedPairsE ees)  =  show ees

-- | Expression of 'Ints' type.
newtype IntsE  =  IntsE { unIntsE :: Expr }

-- | Constant terminal value of 'Ints' type.
newtype IntsE0  =  IntsE0 { unIntsE0 :: Expr }

-- | Varialbe of 'Ints' type.
newtype IntsEV  =  IntsEV { unIntsEV :: Expr }

instance Show Ill where  show (Ill e) = show e

instance Listable IntE  where
  tiers  =  mapT IntE
         $  cons0 i_
         \/ cons1 unIntEV
         \/ cons1 unIntE0
         \/ cons2 (\(IntToIntE f) (IntE xx) -> f :$ xx)
         \/ cons1 (head' . unIntsE) `ofWeight` 2
         \/ cons1 (ord' . unCharE) `ofWeight` 2

instance Listable IntE0 where
  tiers  =  (IntE0 . val) `mapT` (tiers :: [[Int]])

instance Listable IntEV where
  list  =  map IntEV $ listVars "x" (undefined :: Int)

instance Listable IntToIntE where
  tiers  =  mapT IntToIntE
         $  cons0 idE
         \/ cons0 negateE `addWeight` 1
         \/ cons0 absE    `addWeight` 1
         \/ cons2 (\(IntToIntToIntE ef) (IntE ex) -> ef :$ ex)
         \/ toTiers (listVars "f" (undefined :: Int -> Int)) `addWeight` 2

instance Listable IntToIntToIntE where
  list  =  map IntToIntToIntE [plus, times]

instance Listable IntsE  where
  tiers  =  mapT IntsE
         $  cons0 is_
         \/ cons1 unIntsEV
         \/ cons1 unIntsE0
         \/ cons2 (\(IntE ex) (IntsE exs) -> ex -:- exs)
         \/ cons1 (tail' . unIntsE) `ofWeight` 2
         \/ cons2 (\(IntsE exs) (IntsE eys) -> exs -++- eys) `ofWeight` 2
         \/ cons1 (\(IntsE exs) -> sort' exs) `ofWeight` 3
         \/ cons2 (\(IntE ex) (IntsE exs) -> insert' ex exs) `ofWeight` 3

instance Listable IntsE0 where
  tiers  =  (IntsE0 . val) `mapT` (tiers :: [[ [Int] ]])

instance Listable IntsEV where
  list  =  map IntsEV $ listVars "xs" (undefined :: [Int])

instance Listable BoolE  where
  tiers  =  mapT BoolE
         $  cons0 b_
         \/ cons1 unBoolEV
         \/ cons1 unBoolE0
         \/ cons2 (\(BoolToBoolE ef) (BoolE ep) -> ef :$ ep)
         \/ cons2 ((-==-) `on` unIntE)  `addWeight` 2
         \/ cons2 ((-==-) `on` unBoolE) `addWeight` 2
         \/ cons2 ((-<=-) `on` unIntE)  `addWeight` 3
         \/ cons2 ((-<=-) `on` unBoolE) `addWeight` 3
         \/ cons2 ((-<-)  `on` unIntE)  `addWeight` 4
         \/ cons2 ((-<-)  `on` unBoolE) `addWeight` 4
         \/ cons2 ((-/=-) `on` unIntE)  `addWeight` 5
         \/ cons2 ((-/=-) `on` unBoolE) `addWeight` 5
         \/ cons1 (odd'  . unIntE) `addWeight` 2
         \/ cons1 (even' . unIntE) `addWeight` 2
         \/ cons2 (\(IntE ex) (IntsE exs) -> elem' ex exs) `addWeight` 2

instance Listable BoolE0 where
  tiers  =  (BoolE0 . val) `mapT` (tiers :: [[Bool]])

instance Listable BoolEV where
  list  =  map BoolEV $ listVars "p" (undefined :: Bool)

instance Listable BoolToBoolE where
  tiers  =  mapT BoolToBoolE
         $  cons0 notE
         \/ cons2 (\(BoolToBoolToBoolE ef) (BoolE ex) -> ef :$ ex)

instance Listable BoolToBoolToBoolE where
  list  =  map BoolToBoolToBoolE [orE, andE, impliesE]

instance Listable CharE where
  tiers  =  mapT CharE $ cons0 c_
                      \/ cons1 unCharEV
                      \/ cons1 unCharE0

instance Listable CharEV where
  list  =  map CharEV $ listVars "c" (undefined :: Char)

instance Listable CharE0 where
  tiers  =  (CharE0 . val) `mapT` (tiers :: [[Char]])

instance Listable SameTypeE where
  tiers = cons1 (\(IntE  e1, IntE  e2) -> SameTypeE e1 e2) `ofWeight` 0
       \/ cons1 (\(BoolE e1, BoolE e2) -> SameTypeE e1 e2) `ofWeight` 1
       \/ cons1 (\(IntsE e1, IntsE e2) -> SameTypeE e1 e2) `ofWeight` 1
       \/ cons1 (\(CharE e1, CharE e2) -> SameTypeE e1 e2) `ofWeight` 2
       \/ cons1 (\(IntToIntE e1, IntToIntE e2)     -> SameTypeE e1 e2) `ofWeight` 2
       \/ cons1 (\(BoolToBoolE e1, BoolToBoolE e2) -> SameTypeE e1 e2) `ofWeight` 2
       \/ cons1 (\(BoolToBoolToBoolE e1, BoolToBoolToBoolE e2) -> SameTypeE e1 e2) `ofWeight` 2
       \/ cons1 (\(IntToIntToIntE e1, IntToIntToIntE e2)       -> SameTypeE e1 e2) `ofWeight` 2

instance Listable SameTypedPairsE where
  tiers = cons1 (SameTypedPairsE . map unSameTypeE) `ofWeight` 0

instance Listable E0 where
  tiers  =  mapT E0
         $  cons1 unIntE0  `ofWeight` 0
         \/ cons1 unBoolE0 `ofWeight` 1
         \/ cons1 unIntsE0 `ofWeight` 1

instance Listable EV where
  tiers  =  mapT EV
         $  cons1 unIntEV  `ofWeight` 0
         \/ cons1 unBoolEV `ofWeight` 1
         \/ cons1 unIntsEV `ofWeight` 1


instance Listable Expr where
  tiers  =  reset (cons1 unIntE)
         \/ cons1 unBoolE
         \/ cons1 unCharE
         \/ cons1 unIntsE
         \/ cons1 unIntToIntE         `addWeight` 1
         \/ cons1 unIntToIntToIntE    `addWeight` 1
         \/ cons1 unBoolToBoolE       `addWeight` 2
         \/ cons1 unBoolToBoolToBoolE `addWeight` 2


-- | This listable instance only produces Ill typed expressions
instance Listable Ill where
  tiers  =  mapT Ill
         $  cons2 (\(IntE ef) (IntE ex) -> ef :$ ex) `ofWeight` 0
         \/ cons2 (\(IntToIntE ef) (IntToIntE ex) -> ef :$ ex)
         \/ cons2 (\(Ill ef) ex -> ef :$ ex)
         \/ cons2 (\ef (Ill ex)-> ef :$ ex)


instance ShowFunction Expr where bindtiers  =  bindtiersShow
