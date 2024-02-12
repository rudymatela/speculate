{-# LANGUAGE CPP #-}
-- |
-- Module      : Test.Speculate.Utils.List
-- Copyright   : (c) 2016-2019 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of Speculate.
--
-- Utilities for manipulating lists.
module Test.Speculate.Utils.List
  ( pairsThat
  , count, counts, countsOn, countsBy
  , firsts
  , nubSort, nubSortBy
  , (+++), nubMerge, nubMergeBy, nubMergeOn, nubMerges, nubMergesBy, nubMergeMap
  , ordIntersect, ordIntersectBy
  , ordered, orderedBy, orderedOn, strictlyOrdered, strictlyOrderedOn
  , areAll, areAny
  , allLater
  , (+-)
  , sortOn
  , groupOn
  , collectOn, collectBy, collectWith, collectSndByFst
  , discard, discardLater, discardEarlier, discardOthers, discardByOthers
  , allUnique
  , chain
  , zipWithReverse
  , medianate
  , takeGreaterHalf
  , accum
  , partitionByMarkers
  , (!)
  , halve
  , none
  )
where

import Data.List
import Data.Function (on)
import Test.LeanCheck.Stats
import Data.Express.Utils.List (none, nubSort, nubSortBy, (+++))

pairsThat :: (a -> a -> Bool) -> [a] -> [(a,a)]
pairsThat p xs = [(x,y) | x <- xs, y <- xs, p x y]

count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)

firsts :: Eq a => [a] -> [a]
firsts [] = []
firsts (x:xs) = x : firsts (filter (/= x) xs)

halve :: [a] -> ([a],[a])
halve [] = ([],[])
halve xs = (take h xs, drop h xs)
  where
  h = length xs `div` 2

nubMergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
nubMergeBy cmp (x:xs) (y:ys) = case x `cmp` y of
                                 LT -> x:nubMergeBy cmp xs (y:ys)
                                 GT -> y:nubMergeBy cmp (x:xs) ys
                                 EQ -> x:nubMergeBy cmp xs ys
nubMergeBy _ xs ys = xs ++ ys

nubMergeOn :: Ord b => (a -> b) -> [a] -> [a] -> [a]
nubMergeOn f = nubMergeBy (compare `on` f)

nubMerge :: Ord a => [a] -> [a] -> [a]
nubMerge = nubMergeBy compare

ordIntersectBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
ordIntersectBy cmp (x:xs) (y:ys) = case x `cmp` y of
                                     LT -> ordIntersectBy cmp xs (y:ys)
                                     GT -> ordIntersectBy cmp (x:xs) ys
                                     EQ -> x:ordIntersectBy cmp xs ys
ordIntersectBy _ xs ys = []

ordIntersect :: Ord a => [a] -> [a] -> [a]
ordIntersect = ordIntersectBy compare

nubMerges :: Ord a => [[a]] -> [a]
nubMerges = nubMergesBy compare

nubMergesBy :: Ord a => (a -> a -> Ordering) -> [[a]] -> [a]
nubMergesBy cmp [] = []
nubMergesBy cmp [xs] = xs
nubMergesBy cmp xss = nubMergeBy cmp (nubMerges yss) (nubMerges zss)
  where
  (yss,zss) = splitHalf xss
  splitHalf xs = splitAt (length xs `div` 2) xs

nubMergeMap :: Ord b => (a -> [b]) -> [a] -> [b]
nubMergeMap f = nubMerges . map f

orderedBy :: (a -> a -> Bool) -> [a] -> Bool
orderedBy (<) (x:y:xs) = x < y && orderedBy (<) (y:xs)
orderedBy _ _ = True

orderedOn :: Ord b => (a -> b) -> [a] -> Bool
orderedOn f = orderedBy (<=) . map f

ordered :: Ord a => [a] -> Bool
ordered = orderedBy (<=)

strictlyOrderedOn :: Ord b => (a -> b) -> [a] -> Bool
strictlyOrderedOn f = orderedBy (<) . map f

strictlyOrdered :: Ord a => [a] -> Bool
strictlyOrdered = orderedBy (<)

areAll :: [a] -> (a -> Bool) -> Bool
xs `areAll` p = all p xs

areAny :: [a] -> (a -> Bool) -> Bool
xs `areAny` p = any p xs

allLater :: (a -> a -> Bool) -> [a] -> Bool
allLater (<) (x:xs) = all (< x) xs && allLater (<) xs
allLater _ _ = True

-- | @xs +- ys@ superimposes @xs@ over @ys@.
--
-- [1,2,3] +- [0,0,0,0,0,0,0] == [1,2,3,0,0,0,0]
-- [x,y,z] +- [a,b,c,d,e,f,g] == [x,y,z,d,e,f,g]
-- "asdf" +- "this is a test" == "asdf is a test"
(+-) :: Eq a => [a] -> [a] -> [a]
xs +- ys = xs ++ drop (length xs) ys

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy ((==) `on` f)

#if __GLASGOW_HASKELL__ < 710
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f = sortBy (compare `on` f)
#endif

-- TODO: rename this to classify!
collectOn :: Ord b => (a -> b) -> [a] -> [[a]]
collectOn f = groupOn f . sortOn f

collectBy :: (a -> a -> Ordering) -> [a] -> [[a]]
collectBy cmp = groupBy (===) . sortBy cmp
  where
  x === y = x `cmp` y == EQ

collectWith :: Ord b
            => (a -> b) -> (a -> c) -> (b -> [c] -> d)
            -> [a] -> [d]
collectWith f g h = map collapse
                  . groupOn f
  where
  collapse (x:xs) = f x `h` map g (x:xs)
  collapse _      = error "collectWith: the impossible happened! (see source)"

collectSndByFst :: Ord a => [(a,b)] -> [(a,[b])]
collectSndByFst = collectWith fst snd (,)

discard :: (a -> Bool) -> [a] -> [a]
discard p = filter (not . p)

discardLater :: (a -> a -> Bool) -> [a] -> [a]
discardLater d []     = []
discardLater d (x:xs) = x : discardLater d (discard (`d` x) xs)

discardEarlier :: (a -> a -> Bool) -> [a] -> [a]
discardEarlier d = reverse . discardLater d . reverse

discardOthers :: (a -> a -> Bool) -> [a] -> [a]
discardOthers d = dis []
  where
  dis xs []     = xs
  dis xs (y:ys) = dis (y:discard (`d` y) xs) (discard (`d` y) ys)

discardByOthers :: (a -> [a] -> Bool) -> [a] -> [a]
discardByOthers f = d []
  where
  d xs [] = xs
  d xs (y:ys) | f y (xs ++ ys) = d xs        ys
              | otherwise      = d (xs++[y]) ys

allUnique :: Ord a => [a] -> Bool
allUnique xs = nubSort xs == sort xs

chain :: [a -> a] -> a -> a
chain = foldr (.) id

zipWithReverse :: (a -> a -> b) -> [a] -> [b]
zipWithReverse f xs = zipWith f xs (reverse xs)

-- bad name, can't think of something better
medianate :: (a -> a -> b) -> [a] -> [b]
medianate f xs = zipWith f (takeGreaterHalf xs) (takeGreaterHalf $ reverse xs)

takeGreaterHalf :: [a] -> [a]
takeGreaterHalf xs = take (uncurry (+) $ length xs `divMod` 2) xs

accum :: Num a => [a] -> [a]
accum = a 0
  where
  a _ []     = []
  a s (x:xs) = s+x : a (s+x) xs

-- partitionByMarkers x y [x,a,b,c,y,d,e,f,x,g] == ([a,b,c,g],[d,e,f])
partitionByMarkers :: Eq a => a -> a -> [a] -> ([a],[a])
partitionByMarkers y z xs =
  case span (\x -> x /= y && x /= z) xs of
    (ys,[])   -> (ys,[])
    (ys,x:zs)
      | x == y -> let (ys',zs') = partitionByMarkers y z zs in (ys++ys',zs')
      | x == z -> let (zs',ys') = partitionByMarkers z y zs in (ys++ys',zs')
      | otherwise -> error "partitionByMarkers: the impossible happened, this is definitely a bug.  See source."

-- Total version of !! that works on lists of lists (returning [] for index not
-- found).
(!) :: [[a]] -> Int -> [a]
(xs:xss) ! 0  =  xs
(xs:xss) ! n  =  xss ! (n-1)
[]       ! n  =  []
