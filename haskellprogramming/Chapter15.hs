module Chapter15 where

import Data.Monoid
import Control.Monad

import Test.Hspec
import Test.QuickCheck

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool 
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool 
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool 
monoidRightIdentity a = (a <> mempty) == a


data Optional a = 
    Nada
    | Only a
    deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where 
    mempty = Nada
    mappend Nada (Only x) = Only x
    mappend (Only x) Nada = Only x
    mappend Nada Nada = Nada
    mappend (Only x) (Only y) = Only $ mappend x y


-- Don't forget to write an Arbitrary
-- instance for First'. We won't always
-- stub that out explicitly for you.
newtype First' a =
    First' { getFirst' :: Optional a } deriving (Eq, Show)

instance Monoid (First' a) where 
    mempty = First' Nada
    mappend (First' (Only x)) _ = First' (Only x)
    mappend (First' Nada) x = x

instance Arbitrary a => Arbitrary (First' a) where 
    arbitrary = do
        x <- arbitrary
        frequency [ (1, return (First' Nada)) , (1, return (First' (Only x))) ]

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend =
     First' String
  -> First' String
  -> First' String
  -> Bool

type FstId =
  First' String -> Bool

{-
stack --resolver lts-8.15 ghci --package hspec QuickCheck
:l CipherTest.hs Cipher.hs
-}

main :: IO () 
main = do
    quickCheck (monoidAssoc :: FirstMappend) 
    quickCheck (monoidLeftIdentity :: FstId) 
    quickCheck (monoidRightIdentity :: FstId)






