module Chapter21 where

import Data.Monoid
import Data.Foldable
import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


data Query     = Query
data SomeObj   = SomeObj
data IoOnlyObj = IoOnlyObj
data Err       = Err


-- There's a decoder function that makes
-- some object from String
decodeFn :: String -> Either Err SomeObj
decodeFn = undefined
-- There's a query, that runs against DB and
-- returns array of strings
fetchFn :: Query -> IO [String]
fetchFn = undefined
-- there's some additional "context initializer",
-- that also has IO side-effects
makeIoOnlyObj :: [SomeObj] -> IO [(SomeObj, IoOnlyObj)]
makeIoOnlyObj = undefined



pipelineFn :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn = ((traverse makeIoOnlyObj . traverse decodeFn) =<<) . fetchFn

-------------------------------------

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Foldable Identity where
  foldMap f (Identity a) = f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

instance Eq a => EqProp (Identity a) where
  a =-= b = eq a b

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a

-----------------------------------

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap f (Constant a) = Constant a

instance Foldable (Constant a) where
  foldMap _ _ = mempty

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = do
    a <- arbitrary
    return $ Constant a

instance Eq a => EqProp (Constant a b) where
  a =-= b = eq a b

instance Traversable (Constant a) where
  traverse _ (Constant a) = pure $ Constant a

--------------------------

data Optional a = Nada | Yep a deriving (Eq, Ord, Show)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep $ f a

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = do
    a <- arbitrary
    return $ Yep a

instance Eq a => EqProp (Optional a) where
  a =-= b = eq a b

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep a) = Yep <$> f a

--------------

data List a = Nil | Cons a (List a) deriving (Eq, Ord, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons a as) = f a <> foldMap f as

take' :: Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' n (Cons a as) = Cons a (take' (n - 1) as)

-- http://codingstruggles.com/haskell/arbitrary-length-lists-quickcheck.html
arbitraryList :: Arbitrary a => Int -> Gen (List a)
arbitraryList m
    | m == 0 = return Nil
    | m > 6 = arbitraryList 6
    | otherwise = Cons <$> arbitrary <*> arbitraryList (m-1)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = sized arbitraryList

instance Eq a => EqProp (List a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = take' 3000 xs
          ys' = take' 3000 ys

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons a as) = liftA2 Cons (f a) (traverse f as)

---------------------------------------------

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b $ f c

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  a =-= b = eq a b

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c


--------------------------------------------

data Three' a b = Three' a b b deriving (Eq, Show)


instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance Foldable (Three' a) where
  foldMap f (Three' a b c) = f b <> (f c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    b' <- arbitrary
    return $ Three' a b b'

instance (Eq a, Eq b) => EqProp (Three' a b) where
  a =-= b = eq a b

instance Traversable (Three' a) where
  traverse f (Three' a b c) = Three' a <$> (f b) <*> (f c)


-------------------------------------------

data S n a = S (n a) a deriving (Eq, Show)

instance Functor n => Functor (S n) where
  fmap f (S n a) = S (fmap f n) $ f a

instance Foldable n => Foldable (S n) where
  foldMap f (S n a) = foldMap f n <> (f a)


instance (Arbitrary a, Arbitrary (n a)) => Arbitrary (S n a) where
    arbitrary = S <$> arbitrary <*> arbitrary

instance (Eq a, Eq (n a)) => EqProp (S n a) where (=-=) = eq


instance Traversable n => Traversable (S n) where
  traverse f (S n a) = S <$> traverse f n <*> f a

------------------------------

data Tree a = Empty
  | Leaf a
  | Node (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Node l a r) = Node (fmap f l) (f a) (fmap f r)

  -- foldMap is a bit easier and looks more natural,
  -- but you can do foldr too for extra credit.
instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node l a r) = (foldMap f l) <> (f a) <> (foldMap f r)

arbitraryLevelTree :: Arbitrary a => Int -> Int -> Gen (Tree a)
arbitraryLevelTree l r
    | l < 0 && r < 0 = return Empty
    | l == 0 && r == 0 = Leaf <$> arbitrary
    | l > 6 = arbitraryLevelTree 6 r
    | r > 6 = arbitraryLevelTree l 6
    | otherwise = Node <$> arbitraryLevelTree (l-1) (r-1) <*> arbitrary <*> arbitraryLevelTree (l-1) (r-1)

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = do
    l <- sized pure
    r <- sized pure
    arbitraryLevelTree l r


instance Eq a => EqProp (Tree a) where (=-=) = eq


instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node l a r) = Node <$> (traverse f l) <*> (f a) <*> (traverse f r)

main = do
quickBatch (traversable (undefined :: Identity (String, Int, [Int])))
quickBatch (traversable (undefined :: Constant Int (String, Int, [Int])))
quickBatch (traversable (undefined :: Optional (String, Int, [Int])))
quickBatch (traversable (undefined :: List (Int, String, String)))
quickBatch (traversable (undefined :: Three Int String (Int, String, String)))
quickBatch (traversable (undefined :: Three' Int (Int, String, String)))
quickBatch (traversable (undefined :: S Maybe (Int, String, String)))
quickBatch (traversable (undefined :: Tree (Int, String, String)))
