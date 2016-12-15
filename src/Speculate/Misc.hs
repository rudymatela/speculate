-- | Miscellaneous functions I still did not find a reasonable place to put
--   them in.
module Speculate.Misc
  ( functions1
  , functions2
  , functions3
  , functions4
  , fillings

  , expressionsOf
  , valuedExpressionsOf
  )
where

import Speculate
import Speculate.Utils
import Data.Dynamic
import Test.LeanCheck

functions1 :: (Typeable a, Typeable b) => Expr -> [(Expr,a->b)]
functions1 e =
  case l undefined of
    [] -> []
    _  -> fist l
  where
  l = \x -> [(e',v) | e' <- fillings e [x -| "x"], let Just v = evaluate e']

functions2 :: (Typeable a, Typeable b, Typeable c) => Expr -> [(Expr,a->b->c)]
functions2 e =
  case l undefined undefined of
    [] -> []
    _  -> fist2 l
  where
  l = \x y -> [(e',v) | e' <- fillings e [x -| "x", y -| "y"]
                      , let Just v = evaluate e']

functions3 :: (Typeable a, Typeable b, Typeable c, Typeable d)
           => Expr -> [(Expr,a->b->c->d)]
functions3 e =
  case l undefined undefined undefined of
    [] -> []
    _  -> fist3 l
  where
  l = \x y z -> [(e',v) | e' <- fillings e [x -| "x", y -| "y", z -| "z"]
                        , let Just v = evaluate e']

functions4 :: (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e)
           => Expr -> [(Expr,a->b->c->d->e)]
functions4 e =
  case l undefined undefined undefined undefined of
    [] -> []
    _  -> fist4 l
  where
  l = \x y z w -> [(e',v) | e' <- fillings e [x -| "x", y -| "y", z -| "z", w -| "w"]
                          , let Just v = evaluate e']


-- This function is dangerous:
--
-- @f@ should always return the same number of values
-- and should not evaluateuate it's argument when producing the list spine
--
-- fist (function-list), in lack of a better name
fist :: (a->[(z,b)]) -> [(z,a->b)]
fist f = [ (fst $ f' undefined, snd . f')
         | i <- [0..(length (f undefined)-1)]
         , let f' = (!! i) . f ]

fist2 :: (a->b->[(z,c)]) -> [(z,a->b->c)]
fist2 f = map (id *** curry) $ fist (uncurry f)

fist3 :: (a->b->c->[(z,d)]) -> [(z,a->b->c->d)]
fist3 f = map (id *** curry3) $ fist (uncurry3 f)

fist4 :: (a->b->c->d->[(z,e)]) -> [(z,a->b->c->d->e)]
fist4 f = map (id *** curry4) $ fist (uncurry4 f)

-- All possible fillings of holes in an expression:
-- 
-- * For an expression without holes, this returns a singleton list with that
--   expression.
--
-- * If there is no type match between the given filler-expressions,
--   return an empty list.
fillings :: Expr -> [Expr] -> [Expr]
fillings e vs = [fill e f | f <- fs]
  where
  fs = productsList $ [[v | v <- vs, typ v == h] | h <- holes e]

-- | Given a list of atomic expressions, enumerate experssions by application
--
-- NOTE: for now, very inneficient
--
-- This function exists solely for documentation and will never actually be
-- useful, as:
--
-- > mapT fst $ classes
--
-- Will return as expressions that are semantially different (and is more
-- efficient)
--
-- Eventually this function will be removed from Speculate
expressionsOf :: [Expr] -> [[Expr]]
expressionsOf ds = [ds] \/ productMaybeWith ($$) es es `addWeight` 1
  where
  es = expressionsOf ds

-- | Given a list of atomic expressinos, enumerated expressions of a given type
--   by application.
--
--   Never will be actually useful, see 'expressionsOf'.
--
-- Eventually this functino will be removed from Speculate
valuedExpressionsOf :: Typeable a => [Expr] -> [[(Expr,a)]]
valuedExpressionsOf = mapTMaybe exprValue . expressionsOf
  where
  exprValue :: Typeable a => Expr -> Maybe (Expr,a)
  exprValue e = fmap ((,) e) $ evaluate e

