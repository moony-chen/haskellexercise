
{-
stack --resolver lts-8.15 ghci --package hspec QuickCheck semigroups

-}

module Chapter15 where

import Data.Semigroup

import Test.Hspec
import Test.QuickCheck

data Trivial = Trivial deriving (Eq, Show) 

instance Semigroup Trivial where
    _ <> _ = Trivial

instance Arbitrary Trivial where
    arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
    Identity x <> Identity y = Identity (x <> y)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        x <- arbitrary
        return $ Identity x

type IdentityIntegerAssoc = Identity Integer -> Identity Integer -> Identity Integer -> Bool

main :: IO () 
main =
    -- quickCheck (semigroupAssoc :: TrivialAssoc)
    quickCheck (semigroupAssoc :: IdentityIntegerAssoc)