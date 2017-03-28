-- |
-- Module      : Test.Speculate.Utils.Tuple
-- Copyright   : (c) 2016-2017 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
module Test.Speculate.Utils.Tuple
  ( module Data.Tuple
  , fst3, fst4
  , snd3, snd4
  , trd3, trd4
  ,       fth4
  , curry3, curry4
  , uncurry3, uncurry4, uncurry5, uncurry6, uncurry7
  , uncurry8, uncurry9, uncurry10, uncurry11, uncurry12
  , (***)
  , catPairs
  )
where

import Data.Tuple

fst3 :: (a,b,c) -> a
fst3 (x,y,z) = x

snd3 :: (a,b,c) -> b
snd3 (x,y,z) = y

trd3 :: (a,b,c) -> c
trd3 (x,y,z) = z

fst4 :: (a,b,c,d) -> a
fst4 (x,y,z,w) = x

snd4 :: (a,b,c,d) -> b
snd4 (x,y,z,w) = y

trd4 :: (a,b,c,d) -> c
trd4 (x,y,z,w) = z

fth4 :: (a,b,c,d) -> d
fth4 (x,y,z,w) = w

curry3 :: ((a,b,c)->d) -> a -> b -> c -> d
curry3 f x y z = f (x,y,z)

curry4 :: ((a,b,c,d)->e) -> a -> b -> c -> d -> e
curry4 f x y z w = f (x,y,z,w)

uncurry3 :: (a->b->c->d) -> (a,b,c) -> d
uncurry3 f t = f (fst3 t) (snd3 t) (trd3 t)

uncurry4 :: (a->b->c->d->e) -> (a,b,c,d) -> e
uncurry4 f q = f (fst4 q) (snd4 q) (trd4 q) (fth4 q)

uncurry5 :: (a->b->c->d->e->f) -> (a,b,c,d,e) -> f
uncurry5 f (x,y,z,w,v) = f x y z w v

uncurry6 :: (a->b->c->d->e->f->g) -> (a,b,c,d,e,f) -> g
uncurry6 f (x,y,z,w,v,u) = f x y z w v u

uncurry7 :: (a->b->c->d->e->f->g->h) -> (a,b,c,d,e,f,g) -> h
uncurry7 f (x,y,z,w,v,u,r) = f x y z w v u r

uncurry8 :: (a->b->c->d->e->f->g->h->i) -> (a,b,c,d,e,f,g,h) -> i
uncurry8 f (x,y,z,w,v,u,r,s) = f x y z w v u r s

uncurry9 :: (a->b->c->d->e->f->g->h->i->j) -> (a,b,c,d,e,f,g,h,i) -> j
uncurry9 f (x,y,z,w,v,u,r,s,t) = f x y z w v u r s t

uncurry10 :: (a->b->c->d->e->f->g->h->i->j->k) -> (a,b,c,d,e,f,g,h,i,j) -> k
uncurry10 f (x,y,z,w,v,u,r,s,t,o) = f x y z w v u r s t o

uncurry11 :: (a->b->c->d->e->f->g->h->i->j->k->l)
          -> (a,b,c,d,e,f,g,h,i,j,k) -> l
uncurry11 f (x,y,z,w,v,u,r,s,t,o,p) = f x y z w v u r s t o p

uncurry12 :: (a->b->c->d->e->f->g->h->i->j->k->l->m)
          -> (a,b,c,d,e,f,g,h,i,j,k,l) -> m
uncurry12 f (x,y,z,w,v,u,r,s,t,o,p,q) = f x y z w v u r s t o p q

(***) :: (a -> b) -> (c -> d) -> (a,c) -> (b,d)
f *** g = \(x,y) -> (f x, g y)

catPairs :: [(a,a)] -> [a]
catPairs [] = []
catPairs ((x,y):xys) = x:y:catPairs xys
