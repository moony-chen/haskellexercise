
{-
stack --resolver lts-8.15 ghci --package hspec QuickCheck semigroups

-}

module Chapter15 where

import Data.Semigroup

import Test.Hspec
import Test.QuickCheck hiding (Success, Failure)

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)



data Trivial = Trivial deriving (Eq, Show) 

instance Semigroup Trivial where
    _ <> _ = Trivial

instance Arbitrary Trivial where
    arbitrary = return Trivial


type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

-------------------------------------------

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
    Identity x <> Identity y = Identity (x <> y)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        x <- arbitrary
        return $ Identity x

type IdentityAssoc a = Identity a -> Identity a -> Identity a -> Bool

----------------------------------------------

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    Two a b <> Two a' b' = Two (a <> a') (b <> b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Two a b

type TwoAssoc a b = Two a b -> Two a b -> Two a b -> Bool

----------------------------------------------

data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
    Three a b c <> Three a' b' c' = Three (a <> a') (b <> b') (c <> c')

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ Three a b c

type ThreeAssoc a b c = Three a b c -> Three a b c -> Three a b c -> Bool


----------------------------------------------

data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
    Four a b c d <> Four a' b' c' d' = Four (a <> a') (b <> b') (c <> c') (d <> d')

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return $ Four a b c d

type FourAssoc a b c d = Four a b c d -> Four a b c d -> Four a b c d -> Bool

---------------------------------------------

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
    BoolConj True <> BoolConj True = BoolConj True
    _ <> _ = BoolConj False

instance Arbitrary BoolConj where
    arbitrary = elements [BoolConj True, BoolConj False]

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

---------------------------------------------

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
    BoolDisj False <> BoolDisj False = BoolDisj False
    _ <> _ = BoolDisj True

instance Arbitrary BoolDisj where
    arbitrary = elements [BoolDisj True, BoolDisj False]

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

---------------------------------------------

data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
    Fst a <> x = x
    Snd a <> _ = Snd a
    
instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        frequency [(1, return (Fst a)), 
                   (1, return (Snd b))]

type OrAssoc a b = Or a b -> Or a b -> Or a b -> Bool

----------------------------------------

newtype Combine a b = Combine { unCombine :: (a -> b) }

instance (Semigroup b) => Semigroup (Combine a b) where
    Combine g <> Combine h = Combine $ \x -> g x <> h x

-- instance CoArbitrary (Combine a b) where
--     coarbitrary = 

--------------------------------------
newtype Comp a = Comp { unComp :: (a -> a) }

instance (Semigroup a) => Semigroup (Comp a) where
    Comp g <> Comp h = Comp $ \x -> g x <> h x
-------------------------------------
data Validation a b =
    Failure a | Success b
        deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
    Success a <> Success b = Success b
    Failure a <> Success b = Failure a
    Success a <> Failure b = Failure b
    Failure a <> Failure b = Failure $ a <> b

--------------------------------------

newtype AccumulateRight a b = AccumulateRight (Validation a b) deriving (Eq, Show)

instance Semigroup b => Semigroup (AccumulateRight a b) where
    AccumulateRight (Success a) <> AccumulateRight (Success b) = AccumulateRight (Success (a <> b))
    AccumulateRight (Failure a) <> AccumulateRight (Success b) = AccumulateRight (Failure a)
    AccumulateRight (Success a) <> AccumulateRight (Failure b) = AccumulateRight (Failure b)
    AccumulateRight (Failure a) <> AccumulateRight (Failure b) = AccumulateRight (Failure a)

--------------------------------------

newtype AccumulateBoth a b = AccumulateBoth (Validation a b) deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
    AccumulateBoth (Success a) <> AccumulateBoth (Success b) = AccumulateBoth (Success (a <> b))
    AccumulateBoth (Failure a) <> AccumulateBoth (Success b) = AccumulateBoth (Failure a)
    AccumulateBoth (Success a) <> AccumulateBoth (Failure b) = AccumulateBoth (Failure b)
    AccumulateBoth (Failure a) <> AccumulateBoth (Failure b) = AccumulateBoth (Failure (a <> b))


-----------------------------------------------------

main :: IO () 
main = hspec $ do
    describe "Trivial Semigroup" $ do
        it "should has associtivity" $ do
            quickCheck (semigroupAssoc :: TrivialAssoc)
    describe "Identity a Semigroup" $ do
        it "should has associtivity" $ do
            quickCheck (semigroupAssoc :: IdentityAssoc Trivial)
    describe "Two Three Four as Semigroup" $ do
        it "should has associtivity" $ do
            quickCheck (semigroupAssoc :: TwoAssoc Trivial Trivial)
            quickCheck (semigroupAssoc :: ThreeAssoc Trivial Trivial Trivial)
            quickCheck (semigroupAssoc :: FourAssoc Trivial Trivial Trivial Trivial)
    describe "BoolConj BoolDisj Semigroup" $ do
        it "should has associtivity" $ do            
            quickCheck (semigroupAssoc :: BoolConjAssoc)
            quickCheck (semigroupAssoc :: BoolDisjAssoc)
    describe "Or a b Semigroup" $ do
        it "should has associtivity" $ do   
            quickCheck (semigroupAssoc :: OrAssoc Int String)
{-    describe "Combine a b Semigroup" $ do
        it "should calculate" $ do
            let f = Combine $ \n -> Sum (n + 1)
                g = Combine $ \n -> Sum (n - 1) 
            in (unCombine (f <> g) 0) `shouldBe` (Sum 0)-}