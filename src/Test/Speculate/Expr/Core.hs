module Test.Speculate.Expr.Core
  ( Expr (..)
  -- * Smart constructors
  , constant
  , showConstant
  , var
  , hole
  , holeOfTy
  , ($$)

  -- * Smart destructors
  , evaluate
  , eval
  , typ
  , etyp 

  -- * Queries
  , typeCorrect
  , arity
  , holes
  , vars
  , consts
  , atomicConstants
  , subexprs
  , subexprsV
  , isSub
  , hasVar
  , unfoldApp
  , isConstantNamed

  -- * Properties of expressions
  , lengthE
  , countVar
  , countVars
  , unrepeatedVars
  , isAssignment
  , lexicompare
  , compareComplexity

  -- * Useful expressions
  , falseE

  -- * Showing
  , showExpr
  , showPrecExpr
  , showsPrecExpr
  , showOpExpr
  , showsOpExpr
  , eqExprCommuting
  )
where

import Data.List (intercalate, find)
import Data.Maybe (fromMaybe, isJust, catMaybes)
import Data.Function (on)
import Data.Monoid ((<>))

import Data.Dynamic
import Test.LeanCheck
import Test.Speculate.Utils


data Expr = Constant String Dynamic
          | Var String TypeRep
          | Expr :$ Expr

constant :: Typeable a => String -> a -> Expr
constant s x = Constant s (toDyn x)

-- | @showConstant val@ returns a named 'Expr' for a given 'Show' type
showConstant :: (Typeable a, Show a) => a -> Expr
showConstant x = constant (show x) x

-- | @var "x" (undefined :: Ty)@ returns a variable of type 'Ty' named "x"
var :: (Listable a, Typeable a) => String -> a -> Expr
var s a = Var s (typeOf a)

-- | @hole (undefined :: Ty)@ returns a hole of type 'Ty'
--
-- By convention, a Hole is a variable named with the empty string.
hole :: (Listable a, Typeable a) => a -> Expr
hole = holeOfTy . typeOf

holeOfTy :: TypeRep -> Expr
holeOfTy = Var ""

-- | 'Just' an 'Expr' application if the types match,
--   'Nothing' otherwise.
($$) :: Expr -> Expr -> Maybe Expr
e1 $$ e2 =
  case typ e1 `funResultTy` typ e2 of
    Nothing -> Nothing
    Just _  -> Just $ e1 :$ e2


-- Deprecated smart constructors:



-- quick and dirty show instance
instance Show Expr where
  showsPrec d e = showParen (d > 10)
                $ showsPrecExpr 0 e
                . showString " :: "
                . showsPrec 0 (typ e)
                . showString (showHoles e)
    where
    showHoles e = case holes e of
                    [] -> ""
                    hs -> "  (holes: " ++ (intercalate ", " (map show hs)) ++ ")"

showsPrecExpr :: Int -> Expr -> String -> String
showsPrecExpr d (Constant s _) | atomic s && isInfixedPrefix s = showString $ toPrefix s
showsPrecExpr d (Constant s _) = showParen sp $ showString s
  where sp = if atomic s then isInfix s else maybe True (d >) $ outernmostPrec s
showsPrecExpr d (Var "" _)     = showString "_" -- a hole
showsPrecExpr d (Var s _)      = showParen (isInfix s) $ showString s
showsPrecExpr d ((Constant ":" _ :$ e1) :$ e2) =
  case showsPrecExpr 0 e2 "" of
    "[]" -> showString "[" . showsPrecExpr 0 e1 . showString "]"
    '[':cs -> showString "[" . showsPrecExpr 0 e1 . showString "," . showString cs
    cs -> showParen (d > prec ":")
        $ showsOpExpr ":" e1 . showString ":" . showsOpExpr ":" e2
showsPrecExpr d ((Constant f _ :$ e1) :$ e2)
  | isInfix f = showParen (d > prec f)
              $ showsOpExpr f e1
              . showString " " . showString f . showString " "
              . showsOpExpr f e2
  | otherwise = showParen (d > prec " ")
              $ showString f
              . showString " " . showsOpExpr " " e1
              . showString " " . showsOpExpr " " e2
showsPrecExpr d (Constant f _ :$ e1)
  | isInfix f = showParen True
              $ showsOpExpr f e1 . showString " " . showString f
showsPrecExpr d (e1 :$ e2) = showParen (d > prec " ")
                           $ showsPrecExpr (prec " ") e1
                           . showString " "
                           . showsPrecExpr (prec " " + 1) e2

showsOpExpr :: String -> Expr -> String -> String
showsOpExpr op = showsPrecExpr (prec op + 1)

showOpExpr :: String -> Expr -> String
showOpExpr op = showPrecExpr (prec op + 1)

showPrecExpr :: Int -> Expr -> String
showPrecExpr n e = showsPrecExpr n e ""

showExpr :: Expr -> String
showExpr = showPrecExpr 0

-- Does not evaluate values when comparing, but rather their representation as
-- strings and their types.
instance Eq Expr where (==) = eqExprCommuting []

eqExprCommuting :: [Expr] -> Expr -> Expr -> Bool
eqExprCommuting ces = e
  where
  e (Var s1 t1) (Var s2 t2) = t1 == t2 && s1 == s2
  e (Constant s1 d1) (Constant s2 d2) = dynTypeRep d1 == dynTypeRep d2 && s1 == s2
  e ((ef1 :$ ex1) :$ ey1) ((ef2 :$ ex2) :$ ey2)
    | ef1 == ef2 && ef1 `elem` ces = eqExprCommuting ces ex1 ex2 && eqExprCommuting ces ey1 ey2
                                  || eqExprCommuting ces ex1 ey2 && eqExprCommuting ces ey1 ex2
  e (ef1 :$ ex1)  (ef2 :$ ex2)  = ef1 == ef2 && ex1 == ex2
  e _ _ = False

instance Ord Expr where
  compare = compareComplexity


-- | Compare two expressiosn lexicographically
--
-- 1st their type arity;
-- 2nd their type;
-- 3rd var < constants < apps
-- 4th lexicographic order on names
lexicompare :: Expr -> Expr -> Ordering
lexicompare = cmp
  where
  e1 `cmp` e2 | typ e1 /= typ e2 = if arity e1 /= arity e2
                                     then arity e1 `compare` arity e2
                                     else   typ e1 `compare`   typ e2
  Var      s1 _ `cmp` Var      s2 _ = s1 `compare` s2
  Constant s1 _ `cmp` Constant s2 _ = s1 `compare` s2
  (f :$ x)      `cmp` (g :$ y)      = f  `compare` g   `thn`  x `compare` y
  (_ :$ _)      `cmp` _             = GT
  _             `cmp` (_ :$ _)      = LT
  _             `cmp` Var _ _       = GT
  Var _ _       `cmp` _             = LT
  -- Var < Constants < Apps


-- | Compares two expressions first by their complexity:
--   1st length;
--   2nd number of variables (more variables is less complex);
--   3nd sum of number of variable occurrences;
--   4rd normal `compare`.
compareComplexity :: Expr -> Expr -> Ordering
compareComplexity = (compare `on` lengthE)
                 <> (flip compare `on` length . vars)
                 <> (flip compare `on` length . repVars)
                 <> (compare `on` length . consts)
                 <> lexicompare

falseE :: Expr
falseE = showConstant False

-- | 'Just' the value of an expression when possible (correct type, no holes),
--   'Nothing' otherwise.
evaluate :: Typeable a => Expr -> Maybe a
evaluate e = v e >>= fromDynamic
  where
  v :: Expr -> Maybe Dynamic
  v (Var      _ _) = Nothing
  v (Constant _ x) = Just x
  v (e1 :$ e2)     = do v1 <- v e1
                        v2 <- v e2
                        dynApply v1 v2

-- | Evaluates an expression when possible (correct type, no holes).
--   Returns a default value otherwise.
eval :: Typeable a => a -> Expr -> a
eval x e = fromMaybe x (evaluate e)

-- | The type of an expression.  This raises errors, but those should not
--   happen if expressions are smart-constructed.
typ :: Expr -> TypeRep
typ (Constant _ d) = dynTypeRep d
typ (Var      _ t) = t
typ (e1 :$ e2) = resultTy (typ e1) -- this silently ignores type mismatches, was:
{-
  case typ e1 `funResultTy` typ e2 of
    Nothing -> error $ "type mismatch, cannot apply "
                    ++ show (typ e1) ++ " to " ++ show (typ e2)
    Just t  -> t
-}

-- | etyp returns either:
--     the Right type
--     a Left expression with holes with the structure of the I'll typed expression
etyp :: Expr -> Either Expr TypeRep
etyp (e1 :$ e2) =
  case (et1,et2) of
    (Right t1, Right t2) ->
      case t1 `funResultTy` t2 of
        Just t  -> Right t
        Nothing -> Left  e
    _ -> Left e
  where
  et1 = etyp e1
  et2 = etyp e2
  ettoe et = case et of Right t -> Var "" t
                        Left  e -> e
  e = ettoe et1 :$ ettoe et2
etyp e = Right (typ e)
-- on error, what's left is an ill typed expression made up entirely of holes
-- this could be a good workaround, but let's think more: cause it is really workaroundish

typeCorrect :: Expr -> Bool
typeCorrect (e1 :$ e2) = typeCorrect e1
                      && typeCorrect e2
                      && isJust (typ e1 `funResultTy` typ e2)
typeCorrect _ = True

-- | Type arity of an 'Expr'
arity :: Expr -> Int
arity = tyArity . typ

-- | List types holes (unamed variables) in an expression
holes :: Expr -> [TypeRep]
holes (e1 :$ e2)  = holes e1 ++ holes e2
holes (Var "" t) = [t]
holes _ = []

-- | List all variables in an expression.
vars :: Expr -> [(TypeRep,String)]
vars (e1 :$ e2) = vars e1 +++ vars e2
vars (Var s t) = [(t,s)]
vars _ = []

atomicConstants :: Expr -> [Expr]
atomicConstants (e1 :$ e2) = atomicConstants e1 +++ atomicConstants e2
atomicConstants e@(Constant _ _) = [e]
atomicConstants _ = []

hasVar :: Expr -> Bool
hasVar (e1 :$ e2) = hasVar e1 || hasVar e2
hasVar (Var s t) = True
hasVar _ = False

-- | List all variables in an expression, in order, with repetitions
repVars :: Expr -> [(TypeRep,String)]
repVars (e1 :$ e2) = repVars e1 ++ repVars e2
repVars (Var s t) = [(t,s)]
repVars _ = []

-- | List terminal constants in an expression.  This does not repeat values.
consts :: Expr -> [Expr]
consts (e1 :$ e2)       = consts e1 +++ consts e2
consts e@(Constant _ _) = [e]
consts _                = []


-- | Returns the length of an expression.  In term rewriting terms: |s|
lengthE :: Expr -> Int
lengthE (e1 :$ e2)  = lengthE e1 + lengthE e2
lengthE _           = 1

-- | Number of occurrences of a given variable name.
-- In term rewriting terms: |s|_x
countVar :: TypeRep -> String -> Expr -> Int
countVar t n (e1 :$ e2) = countVar t n e1 + countVar t n e2
countVar t n (Var n' t') | t == t' && n == n' = 1
countVar _ _ _ = 0

countVars :: Expr -> [(TypeRep,String,Int)]
countVars e = map (\(t,n) -> (t,n,countVar t n e)) $ vars e

unrepeatedVars :: Expr -> Bool
unrepeatedVars = all (\(_,_,n) -> n == 1) . countVars

-- Is this espression an assignment of a variable to a value?
isAssignment :: Expr -> Bool
isAssignment (((Constant "==" _) :$ (Var _ _)) :$ e2) = True
isAssignment (((Constant "==" _) :$ e1) :$ (Var _ _)) = True
isAssignment _ = False

-- | Non-variable sub-expressions of an expression
--
-- This includes the expression itself
subexprs :: Expr -> [Expr]
subexprs e@(e1 :$ e2)     = [e] +++ subexprs e1 +++ subexprs e2
subexprs e@(Constant _ _) = [e]
subexprs _                = []

-- | Sub-expressions of an expression
--   including variables and the expression itself.
subexprsV :: Expr -> [Expr]
subexprsV e@(e1 :$ e2)  = [e] +++ subexprsV e1 +++ subexprsV e2
subexprsV e = [e]

-- | Is a subexpression of.
isSub :: Expr -> Expr -> Bool
isSub e e0 | e == e0 = True
isSub e (e1 :$ e2) = isSub e e1 || isSub e e2
isSub e e0 = e == e0

-- | Make substitutions on subexpressions, variables have to match exactly!
sub :: Expr -> Expr -> Expr -> Expr
sub ef et = s
  where
  s e | e == ef = et
  s (e1 :$ e2)  = s e1 :$ s e2
  s e           = e

isConstantNamed :: Expr -> String -> Bool
Constant n' _ `isConstantNamed` n = n' == n
_             `isConstantNamed` _ = False

-- | Unfold function application:
--
-- > (((f :$ e1) :$ e2) :$ e3) = [f,e1,e2,e3]
unfoldApp :: Expr -> [Expr]
unfoldApp (ef :$ ex) = unfoldApp ef ++ [ex]
unfoldApp  ef        = [ef]
