-- Colin Runciman, December 2016
import Test.Speculate
import Test.LeanCheck
import Data.Function (on)
import Data.List (isSubsequenceOf)

data BT a = Null | Fork (BT a) a (BT a)
            deriving (Eq,Ord,Show)

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

fromList :: Ord a => [a] -> BT a
fromList = foldr insert Null

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

ordered :: Ord a => [a] -> Bool
ordered [] = True
ordered xs = and (zipWith (<=) xs $ tail xs)

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

type Item = Word2

(|==|) :: (Listable a, Eq a, Ord a) => BT a -> BT a -> Bool
(|==|) = (==) `on` toList

(|<=|) :: (Listable a, Eq a, Ord a) => BT a -> BT a -> Bool
(|<=|) = isSubsequenceOf `on` toList

main :: IO ()
main = speculate args
  { instances =
      [ eqWith  ((|==|) :: BT Item -> BT Item -> Bool)
      , ordWith ((|<=|) :: BT Item -> BT Item -> Bool)
      , ins "t" (undefined :: BT Item)
      ]
  , constants =
      [ showConstant (Null :: BT Item)
      , constant "insert" (insert :: Item -> BT Item -> BT Item)
      , constant "delete" (delete :: Item -> BT Item -> BT Item)
      , constant "toList" (toList :: BT Item -> [Item])
      , constant "fromList" (fromList :: [Item] -> BT Item)
      , constant "isSearch" (isSearch :: BT Item -> Bool)
      , constant "ordered" (ordered :: [Item] -> Bool)
      , constant "strictlyOrdered" (strictlyOrdered :: [Item] -> Bool)
      , constant "isIn" (isIn :: Item -> BT Item -> Bool)
      ]
  , backgroundConstants =
      [ constant "<="  ((<=) :: Item -> Item -> Bool)
      , constant "/="  ((/=) :: Item -> Item -> Bool)
--      TODO: when the following is added speculate "breaks" and prints a lot
--            of junk laws.  Prune those away in speculate.  Then re-add not.
--    , constant "not" (not :: Bool -> Bool)
      ]
  }
