module Chapter17 where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)

-------------------------


data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
    fmap f (Pair x x') = Pair (f x) (f x')

instance Applicative Pair where
  pure a = Pair a a
  Pair f f' <*> Pair v v' = Pair (f v) (f' v')

instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary = do
        x <- arbitrary
        x' <- arbitrary
        return $ Pair x x'

instance Eq a => EqProp (Pair a) where
  a =-= b = eq a b
-----------------------------

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
  pure = Two mempty
  Two a f <*> Two a' v = Two (a <> a') (f v)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        x <- arbitrary
        x' <- arbitrary
        return $ Two x x'

instance (Eq a, Eq b) => EqProp (Two a b) where
  a =-= b = eq a b

-----------------------------

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  Three a b f <*> Three a' b' v = Three (a <> a') (b <> b') (f v)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = liftA3 Three arbitrary arbitrary arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  a =-= b = eq a b
-----------------------------

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' a b c) = Three' a (f b) (f c)

instance Monoid a => Applicative (Three' a) where
  pure a = Three' mempty a a
  Three' a f f' <*> Three' a' v v' = Three' (a <> a') (f v) (f' v')

instance (Eq a, Eq b) => EqProp (Three' a b) where
  a =-= b = eq a b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = do
        x <- arbitrary
        x' <- arbitrary
        x'' <- arbitrary
        return $ Three' x x' x''

-----------------------------

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure = Four mempty mempty mempty
  Four a b c f <*> Four a' b' c' v = Four (a <> a') (b <> b') (c <> c') (f v)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = do
        x <- arbitrary
        x' <- arbitrary
        x'' <- arbitrary
        x''' <- arbitrary
        return $ Four x x' x'' x'''

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  a =-= b = eq a b

-----------------------------

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
    fmap f (Four' a b c d) = Four' a b c (f d)

instance Monoid a => Applicative (Four' a) where
  pure a = Four' mempty mempty mempty a
  Four' a a' a'' f <*> Four' b b' b'' v = Four' (a <> b) (a' <> b') (a'' <> b'') (f v)

instance (Eq a, Eq b) => EqProp (Four' a b) where
  a =-= b = eq a b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = do
        x <- arbitrary
        x' <- arbitrary
        x'' <- arbitrary
        x''' <- arbitrary
        return $ Four' x x' x'' x'''



main :: IO ()
main = do
  quickBatch (applicative (undefined :: Pair (Int, String, String)))
  quickBatch (applicative (undefined :: Two (Sum Int) (Int, String, String)))
  quickBatch (applicative (undefined :: Three String (Sum Int) (Int, String, String)))
  quickBatch (applicative (undefined :: Three' String (Int, String, String)))
  quickBatch (applicative (undefined :: Four String (Sum Int) (Product Int) (Int, String, String)))
  quickBatch (applicative (undefined :: Four' String (Int, String, String)))
