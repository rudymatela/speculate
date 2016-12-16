{-# LANGUAGE CPP #-}
module Test.Speculate.Utils.List
  ( pairsThat
  , count, counts, countsBy
  , firsts
  , nubSort
  , (+++), nubMerge, nubMergeBy, nubMergeOn, nubMerges, nubMergeMap
  , ordIntersect, ordIntersectBy
  , ordered, orderedBy, orderedOn, strictlyOrdered, strictlyOrderedOn
  , areAll
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
  )
where

import Data.List
import Data.Function (on)

pairsThat :: (a -> a -> Bool) -> [a] -> [(a,a)]
pairsThat p xs = [(x,y) | x <- xs, y <- xs, p x y]

count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)

counts :: Eq a => [a] -> [(a,Int)]
counts [] = []
counts (x:xs) = (x,1+count x xs) : counts (filter (/= x) xs)

countsBy :: Eq b => (a -> b) -> [a] -> [(b,Int)]
countsBy f = counts . map f

firsts :: Eq a => [a] -> [a]
firsts [] = []
firsts (x:xs) = x : firsts (filter (/= x) xs)

nubSort :: Ord a => [a] -> [a]
nubSort = nub . sort

nubMergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
nubMergeBy cmp (x:xs) (y:ys) = case x `cmp` y of
                                 LT -> x:nubMergeBy cmp xs (y:ys)
                                 GT -> y:nubMergeBy cmp (x:xs) ys
                                 EQ -> x:nubMergeBy cmp xs ys
nubMergeBy _ xs ys = xs ++ ys

nubMergeOn :: Ord b => (a -> b) -> [a] -> [a] -> [a]
nubMergeOn f xs ys = nubMergeBy (compare `on` f) xs ys

nubMerge :: Ord a => [a] -> [a] -> [a]
nubMerge = nubMergeBy compare

(+++) :: Ord a => [a] -> [a] -> [a]
(+++) = nubMerge
infixr 5 +++

ordIntersectBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
ordIntersectBy cmp (x:xs) (y:ys) = case x `cmp` y of
                                     LT -> ordIntersectBy cmp xs (y:ys)
                                     GT -> ordIntersectBy cmp (x:xs) ys
                                     EQ -> x:ordIntersectBy cmp xs ys
ordIntersectBy _ xs ys = []

ordIntersect :: Ord a => [a] -> [a] -> [a]
ordIntersect = ordIntersectBy compare

nubMerges :: Ord a => [[a]] -> [a]
nubMerges [] = []
nubMerges [xs] = xs
nubMerges xss = nubMerges yss `nubMerge` nubMerges zss
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
  where collapse (x:xs) = f x `h` map g (x:xs)

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
allUnique xs = nub (sort xs) == sort xs

chain :: [a -> a] -> a -> a
chain = foldr (.) id

zipWithReverse :: (a -> a -> b) -> [a] -> [b]
zipWithReverse f xs = zipWith f xs (reverse xs)

-- bad name, can't think of something better
medianate :: (a -> a -> b) -> [a] -> [b]
medianate f xs = zipWith f (takeGreaterHalf xs) (takeGreaterHalf $ reverse xs)

takeGreaterHalf :: [a] -> [a]
takeGreaterHalf xs = take (uncurry (+) $ length xs `divMod` 2) xs
