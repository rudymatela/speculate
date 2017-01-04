module Test.Speculate.CondReason where

import Test.Speculate.Expr
import Test.Speculate.Reason
import qualified Test.Speculate.Utils.Digraph as D
import Test.Speculate.Utils.Digraph (Digraph)
import Data.Maybe (mapMaybe,maybeToList)
import Data.List (lookup)
import Data.Functor ((<$>)) -- for GHC < 7.10
import qualified Data.List as L
import Test.Speculate.Utils

-- Chy = Conditional Thy = Conditional Theory
data Chy = Chy
  { cequations :: [(Expr,Expr,Expr)]
  , cimplications :: Digraph Expr
  , cclasses :: [(Expr,[Expr])]
  , unThy :: Thy
  }

emptyChy = Chy
  { cequations = []
  , cimplications = D.empty
  , cclasses = []
  , unThy = emptyThy
  }

updateCEquationsBy :: ([(Expr,Expr,Expr)] -> [(Expr,Expr,Expr)]) -> Chy -> Chy
updateCEquationsBy f chy@Chy{cequations = ceqs} = chy{cequations = f ceqs}

listImplied :: Chy -> Expr -> [Expr]
listImplied Chy{cimplications = ccss} ce = D.succs ce ccss

listImplies :: Chy -> Expr -> [Expr]
listImplies Chy{cimplications = ccss} ce = D.preds ce ccss

listEquivalent :: Chy -> Expr -> [Expr]
listEquivalent Chy{cclasses = ccss} ce =
  case lookup ce ccss of
    Nothing -> [] -- error "error (listEquivalent): cannot find condition in classes"
    Just cs -> cs

reduceRootWith :: Binds -> Expr -> (Expr,Expr) -> Maybe Expr
reduceRootWith bs e (e1,e2) = (e2 `assigning`) <$> (matchWith bs e e1)

reductions1With :: Binds -> Expr -> (Expr,Expr) -> [Expr]
reductions1With bs e (l,_) | lengthE l > lengthE e = [] -- optional optimization
reductions1With bs e@(e1 :$ e2) r = maybeToList (reduceRootWith bs e r)
                                 ++ map (:$ e2) (reductions1With bs e1 r)
                                 ++ map (e1 :$) (reductions1With bs e2 r)
reductions1With bs e r = maybeToList (reduceRootWith bs e r)

creductions1 :: Expr -> Expr -> (Expr,Expr,Expr) -> [Expr]
creductions1 ce e (ceq,el,er) =
  case ce `match` ceq of
    Nothing -> []
    Just bs -> reductions1With bs e (el,er)

-- normalize is maybe a misnomer.  not necessarily convergent.
cnormalize :: Chy -> Expr -> Expr -> Expr
cnormalize chy@Chy{cequations = ceqs, unThy = thy} ce = n
  where
  n e = case filter (canReduceTo thy e)
           $ concatMap (creductions1 ce e) ceqs
          ++ concatMap (\ce' -> concatMap (creductions1 ce' e) ceqs) (listEquivalent chy ce)
          ++ concatMap (\ce' -> concatMap (creductions1 ce' e) ceqs) (listImplied chy ce)
          ++ concatMap (\ce' -> concatMap (creductions1 ce' e) ceqs) (concatMap (listEquivalent chy) (listImplied chy ce)) of
          [] -> e -- already normalized
          (e':_) -> n $ normalize thy e'
-- TODO: fix silly code structure in cnormalize!

cequivalent :: Chy -> Expr -> Expr -> Expr -> Bool
cequivalent chy ce e1 e2 =
  equivalent (unThy chy) (cnormalize chy ce e1) (cnormalize chy ce e2)

cIsInstanceOf :: Chy -> (Expr,Expr,Expr) -> (Expr,Expr,Expr) -> Bool
cIsInstanceOf chy (ce2,le2,re2) (ce1,le1,re1) =
  case match2 (le2,re2) (le1,re1) of
    Nothing -> False
    Just bs -> equivalent (unThy chy) (ce1 `assigning` bs) ce2

-- TODO: make cinsert result independent of insertion order
cinsert :: (Expr,Expr,Expr) -> Chy -> Chy
cinsert ceq@(ce,e1,e2) chy@Chy{cequations = eqs}
  | cequivalent chy ce e1 e2 = chy
  | otherwise = cdelete $ chy {cequations = eqs ++ [ceq]}

cfilter :: ((Expr,Expr,Expr) -> Bool) -> Chy -> Chy
cfilter p = updateCEquationsBy (filter p)

cdiscard :: ((Expr,Expr,Expr) -> Bool) -> Chy -> Chy
cdiscard p = cfilter (not . p)

cdelete :: Chy -> Chy
cdelete chy = updateCEquationsBy upd chy
  where
  upd = id -- discardLater (cIsInstanceOf chy) uneeded
      . discardByOthers (\(ce,e1,e2) eqs -> cequivalent chy{cequations = eqs} ce e1 e2)

cfinalize :: Chy -> Chy
cfinalize chy@Chy{cequations = ceqs} =
  updateCEquationsBy (concatMap $ expandSmallerConditions) chy
  where
  expandSmallerConditions ceq@(ce,e1,e2) =
    (ce,e1,e2) : [ (ce',cnormalize chy' ce' e1,cnormalize chy' ce' e2)
                 | ce' <- listImplies chy ce
                 , lengthE ce' < lengthE ce
                 , ce' /= falseE
                 , let chy' = chy{cequations = L.delete ceq ceqs}
                 , not $ cequivalent chy' ce' e1 e2
                 ]

canonicalizeCEqn :: (Expr,Expr,Expr) -> (Expr,Expr,Expr)
canonicalizeCEqn = canonicalizeCEqnWith basicTypeInfo

canonicalizeCEqnWith :: TypeInfo -> (Expr,Expr,Expr) -> (Expr,Expr,Expr)
canonicalizeCEqnWith ti = c . o
  where
  c (ce,e1,e2) = case canonicalizeWith ti (e2 :$ (e1 :$ ce)) of
                   (e2' :$ (e1' :$ ce')) -> (ce',e1',e2')
                   _ -> error $ "canonicalizeCEqnWith: the impossible happened,"
                             ++ "this is definitely a bug, see source!"
  o (ce,e1,e2) | e1 `compareComplexity` e2 == LT = (ce,e2,e1)
               | otherwise                       = (ce,e1,e2)

canonicalCEqnBy :: TypeInfo -> (Expr,Expr,Expr) -> Bool
canonicalCEqnBy ti ceqn = canonicalizeCEqnWith ti ceqn == ceqn

canonicalCEqn :: (Expr,Expr,Expr) -> Bool
canonicalCEqn = canonicalCEqnBy basicTypeInfo

prettyChy :: ((Expr,Expr,Expr) -> Bool) -> Chy -> String
prettyChy shouldShow =
    table "r r r l l"
  . map (\(pre,e1,e2) -> [ showOpExpr "==>" pre
                         , "==>", showOpExpr "==" e1
                         , "==",  showOpExpr "==" e2 ])
  . filter shouldShow
  . cequations
  . cfinalize
