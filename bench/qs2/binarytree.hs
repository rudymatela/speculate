import QuickSpec hiding (insert)
import Test.QuickCheck hiding (insert,(==>))
import Data.Function (on)
import Data.List (isSubsequenceOf)

data BT a = Null | Fork (BT a) a (BT a)
            deriving (Show)

instance (Eq a, Ord a) => Eq (BT a) where
  (==) = (==) `on` toList

instance (Eq a, Ord a) => Ord (BT a) where
  (<=) = isSubsequenceOf `on` toList

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

instance (Ord a, Arbitrary a) => Arbitrary (BT a) where
  arbitrary = sized arbtree
    where
    arbtree 0 = return Null
    arbtree n = oneof
      [ return Null
      , (Fork <$> arbtree (n `div` 2) <*> arbitrary <*> arbtree (n `div` 2))
        `suchThat` isSearch
      ]

type Item = Int

main = quickSpec signature
  { maxTermSize = Just 9
  , maxTests = Just 2000
-- the maximum number of tests above needs to be 2000,
-- otherwise the law `isIn x t1 ==> isIn x (insert y t1) = True`
-- does not appear in the output (which is mainly what I want with this
-- benchmark).  Even so, it works only about 2/3 of the time.
  , constants =
      [ constant "Null" (Null :: BT Item)
      , constant "insert" (insert :: Item -> BT Item -> BT Item)
      , constant "delete" (delete :: Item -> BT Item -> BT Item)
      , constant "isIn" (isIn :: Item -> BT Item -> Bool)
--    , constant "<="  ((<=) :: Item -> Item -> Bool)
--    , constant "<="  ((<=) :: BT Item -> BT Item -> Bool)
--    , constant "/="  ((/=) :: Item -> Item -> Bool)
--    , constant "ordered" (ordered :: [Item] -> Bool)
--    , constant "strictlyOrdered" (strictlyOrdered :: [Item] -> Bool)
--    , constant "toList" (toList :: BT Item -> [Item])
--    , constant "fromList" (fromList :: [Item] -> BT Item)
--    , constant "isSearch" (isSearch :: BT Item -> Bool)
--    , constant "[]" ([]::[Item])
      , constant "==>" ((==>) :: Bool -> Bool -> Bool)
      , constant "True" True
      , constant "False" False
      ]
  , instances =
      [ baseType (undefined :: BT Item)
      , baseTypeNames ["t1","t2","t3"] (undefined :: BT Item)
      ]
  }

(==>) :: Bool -> Bool -> Bool
False ==> _ = True
True  ==> p = p
