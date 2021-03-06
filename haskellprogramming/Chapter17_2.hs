{-
stack --resolver lts-8.15 ghci --package QuickCheck
:l
-}

module BadMonoid where

import Data.Monoid
import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Bull =
    Fools
  | Twoo
  deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary =
    frequency [ (1, return Fools)
              , (1, return Twoo) ]

instance Monoid Bull where
  mempty = Fools
  mappend _ _ = Fools

instance EqProp Bull where (=-=) = eq


data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

-- write this one in terms of concat' and fmap
flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fmap f as

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

instance Functor List
  where fmap _ Nil = Nil
        fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Applicative List
  where
    pure a = Cons a Nil
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    Cons f fs <*> vs = fmap f vs `append` (fs <*> vs)

instance Eq a => EqProp (List a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = take' 3000 xs
          ys' = take' 3000 ys

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

repeat' :: a -> List a
repeat' a = Cons a (repeat' a)

repeat'' :: a -> ZipList' a
repeat'' a = ZipList' $ repeat' a

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure = repeat''
  ZipList' Nil <*> _ = ZipList' Nil
  _ <*> ZipList' Nil = ZipList' Nil
  ZipList' (Cons f Nil) <*> ZipList' (Cons v vs) = ZipList' $ Cons (f v) Nil
  ZipList' (Cons f fs) <*> ZipList' (Cons v Nil) = ZipList' $ Cons (f v) Nil
  ZipList' (Cons f fs) <*> ZipList' (Cons v vs) = ZipList' $ Cons (f v) fvs
    where ZipList' fvs = ZipList' fs <*> ZipList' vs

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary =  ZipList' <$> sized arbitraryList

instance Arbitrary a => Arbitrary (Sum a) where
  arbitrary = Sum <$> arbitrary

instance Monoid a => Monoid (ZipList' a) where
  mempty = pure mempty
  mappend = liftA2 mappend


data Validation e a =
    Failure' e
  | Success' a
  deriving (Eq, Show)

-- same as Either
instance Functor (Validation e) where
  fmap _ (Failure' e) = Failure' e
  fmap f (Success' a) = Success' $ f a
-- This is different
instance Monoid e => Applicative (Validation e) where
  pure = Success'
  Failure' e <*> Failure' e' = Failure' $ e <> e'
  Failure' e <*> _ = Failure' e
  _ <*> Failure' e = Failure' e
  Success' f <*> Success' v = Success' $ f v

instance (Arbitrary a, Arbitrary e) => Arbitrary (Validation e a) where
  arbitrary =
    frequency [ (1, Failure' <$> arbitrary)
              , (1, Success' <$> arbitrary) ]

instance (Eq e, Eq a) => EqProp (Validation e a) where
  a =-= b = eq a b

main :: IO ()
main = do
  -- quickBatch (monoid Twoo)
  quickBatch (monoid (undefined :: ZipList' (Sum Int)))
  quickBatch (applicative (undefined :: Validation String (Int, String, String)))
  quickBatch (applicative (undefined :: ZipList' (Int, String, String)))
