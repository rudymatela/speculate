{-# LANGUAGE CPP #-}
{-# Language DeriveDataTypeable, StandaloneDeriving #-} -- for GHC <= 7.8
-- Colin Runciman, December 2016
import Test.Speculate
import Test.LeanCheck
import Data.Function (on)


data BT a = Null | Fork (BT a) a (BT a)
            deriving Show

#if __GLASGOW_HASKELL__ < 708
deriving instance Typeable1 BT
#else
deriving instance Typeable BT
#endif

instance (Eq a, Ord a) => Eq (BT a) where
  (==) = (==) `on` toList

instance (Eq a, Ord a) => Ord (BT a) where
  (<=) = isSubsequenceOf `on` toList

isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf []    _  = True
isSubsequenceOf (_:_) [] = False
isSubsequenceOf (x:xs) (y:ys)
  | x == y    =    xs  `isSubsequenceOf` ys
  | otherwise = (x:xs) `isSubsequenceOf` ys

insert :: Ord a => a -> BT a -> BT a
insert x Null             = Fork Null x Null
insert x t@(Fork t1 y t2) = case compare x y of
                            LT -> Fork (insert x t1) y t2
                            EQ -> t
                            GT -> Fork t1 y (insert x t2)

delete :: Ord a => a -> BT a -> BT a
delete x Null = Null
delete x t@(Fork t1 y t2) = case compare x y of
                            LT -> Fork (delete x t1) y t2
                            EQ -> graft t1 t2
                            GT -> Fork t1 y (delete x t2)

isIn :: Ord a => a -> BT a -> Bool
isIn x t = x `elem` toList t

graft :: Ord a => BT a -> BT a -> BT a
graft Null           t = t
graft (Fork t1 x t2) t = Fork t1 x (graft t2 t)

toList :: Ord a => BT a -> [a]
toList Null           = []
toList (Fork t1 x t2) = toList t1 ++ [x] ++ toList t2

--fromList :: Ord a => [a] -> BT a
--fromList = foldr insert Null

{-
fromList :: Ord a => [a] -> BT a
fromList [] = Null
fromList xs = Fork (fromList ys) x (fromList zs)
  where
  (x:ys,zs) = deal xs

deal :: [a] -> ([a],[a])
deal []     = ([],[])
deal (x:xs) = (x:ys,zs)
  where
  (zs,ys) = deal xs
-}

isSearch :: Ord a => BT a -> Bool
isSearch = strictlyOrdered . toList

strictlyOrdered :: Ord a => [a] -> Bool
strictlyOrdered [] = True
strictlyOrdered xs = and (zipWith (<) xs $ tail xs)

-- | truncate tiers of values in the presence of one empty size
--
-- truncateT [[x,y],[z,w],[],[],[],...] == [[x,y],[z,w]]
truncateT :: [[a]] -> [[a]]
truncateT ([]:xss) = []
truncateT (xs:xss) = xs:truncateT xss
truncateT xss = xss

instance (Ord a, Listable a) => Listable (BT a) where
  tiers = truncateT
        $ cons0 Null \/ cons3 Fork `suchThat` isSearch

instance Name (BT a) where name _ = "t"
instance Name Word2 where name _ = "x"

type Item = Word2

main :: IO ()
main = speculate args
  { instances =
      [ reifyInstances (undefined :: BT Item)
      , reifyInstances (undefined :: Item)
      ]
  , constants =
      [ showConstant (Null :: BT Item)
      , constant "insert" (insert :: Item -> BT Item -> BT Item)
      , constant "delete" (delete :: Item -> BT Item -> BT Item)
      , constant "isIn" (isIn :: Item -> BT Item -> Bool)
      , background
      , constant "/="  ((/=) :: Item -> Item -> Bool)
      ]
  }
