{-
stack --resolver lts-8.15 ghci --package hspec QuickCheck
:l 
-}

module Chapter15 where

import Data.Monoid hiding ((<>))
import qualified Data.Monoid as M
import Data.Semigroup
import Test.Hspec
import Test.QuickCheck



data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial

instance Monoid Trivial where
    mempty = Trivial
    mappend = (<>)

instance Arbitrary Trivial where
    arbitrary = return Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool


-------------

newtype Identity a = Identity a deriving Show

instance Eq a => Eq (Identity a) where
    Identity x == Identity y = x == y

instance (Semigroup a) => Semigroup (Identity a) where
    Identity a <> Identity b = Identity (a <> b)

instance (Monoid a, Semigroup a) => Monoid (Identity a) where
    mempty = Identity mempty
    mappend = (<>)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        x <- arbitrary
        return $ Identity x

type IdentityAssoc a = (Identity a) -> (Identity a) -> (Identity a) -> Bool

-------------------------

data Two a b = Two a b deriving Show

instance (Eq a, Eq b) => Eq (Two a b) where
    Two a b == Two a' b' = a == a' && b == b'

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    Two a b <> Two a' b' = Two (a <> a') (b <> b')

instance (Monoid a, Semigroup a, Monoid b, Semigroup b) => Monoid (Two a b) where
    mempty = Two mempty mempty
    mappend = (<>)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        return $ Two x y

type TwoAssoc a b = (Two a b) -> (Two a b) -> (Two a b) -> Bool

----------------------------

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
    BoolConj True <> BoolConj True = BoolConj True
    _ <> _ = BoolConj False 

instance Monoid BoolConj where
    mempty = BoolConj True
    mappend = (<>)

instance Arbitrary BoolConj where
    arbitrary = elements [BoolConj True, BoolConj False]

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

---------------------------

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
    BoolDisj False <> BoolDisj False = BoolDisj False
    _ <> _ = BoolDisj True 

instance Monoid BoolDisj where
    mempty = BoolDisj False
    mappend = (<>)

instance Arbitrary BoolDisj where
    arbitrary = elements [BoolDisj True, BoolDisj False]

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

---------------------------

newtype Combine a b = Combine { unCombine :: (a -> b) }

instance (Semigroup b) => Semigroup (Combine a b) where
    Combine g <> Combine h = Combine $ \x -> g x <> h x

instance (Semigroup b, Monoid b) => Monoid (Combine a b) where
    mempty = Combine $ \x -> mempty
    mappend = (<>)    

---------------------------------


newtype Mem s a =
    Mem {
        runMem :: s -> (a,s)
    }

instance (Semigroup a) => Semigroup (Mem s a) where
    Mem f <> Mem g = Mem $ \s -> ((fst (f s)) <> (fst (g s)),
                                            snd . g . snd . f $ s)

instance (Semigroup a, Monoid a) => Monoid (Mem s a) where
    mempty = Mem $ \s -> (mempty, s)
    mappend = (<>)
      
f' :: Mem Int String
f' = Mem $ \s -> ("hi", s + 1) 

-----------------------------

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty M.<> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a M.<> mempty) == a

main :: IO ()
main = do
    quickCheck (semigroupAssoc :: TrivialAssoc)
    quickCheck (monoidLeftIdentity :: Trivial -> Bool)
    quickCheck (monoidRightIdentity :: Trivial -> Bool)    
    quickCheck (semigroupAssoc :: IdentityAssoc Trivial) 
    quickCheck (monoidLeftIdentity :: (Identity Trivial) -> Bool)
    quickCheck (monoidRightIdentity :: (Identity Trivial) -> Bool)    
    quickCheck (semigroupAssoc :: TwoAssoc Trivial Trivial) 
    quickCheck (monoidLeftIdentity :: (Two Trivial Trivial) -> Bool)
    quickCheck (monoidRightIdentity :: (Two Trivial Trivial) -> Bool)
    quickCheck (semigroupAssoc :: BoolConjAssoc) 
    quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
    quickCheck (monoidRightIdentity :: BoolConj -> Bool)
    quickCheck (semigroupAssoc :: BoolDisjAssoc) 
    quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
    quickCheck (monoidRightIdentity :: BoolDisj -> Bool)

    print $ runMem (f' <> mempty) 0
    print $ runMem (mempty <> f') 0
    print $ (runMem mempty 0 :: (String, Int))
    print $ runMem (f' <> mempty) 0 == runMem f' 0
    print $ runMem (mempty <> f') 0 == runMem f' 0