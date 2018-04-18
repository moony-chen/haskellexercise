{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}

module ReplaceExperiment where


import Test.QuickCheck
import Test.QuickCheck.Function


replaceWithP :: b -> Char
replaceWithP = const 'p'

lms :: [Maybe [Char]]
lms = [Just "Ave", Nothing, Just "woohoo"]

-- Just making the argument more specific
replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP

-- Prelude> :t fmap replaceWithP
-- fmap replaceWithP :: Functor f => f a -> f Char
liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP

liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace

-- Prelude> :t (fmap . fmap) replaceWithP
-- (fmap . fmap) replaceWithP
-- :: (Functor f1, Functor f) => f (f1 a) -> f (f1 Char)
twiceLifted :: (Functor f1, Functor f) => f (f1 a) -> f (f1 Char)
twiceLifted = (fmap . fmap) replaceWithP
-- Making it more specific
twiceLifted' :: [Maybe [Char]] -> [Maybe Char]
twiceLifted' = twiceLifted
-- f ~ []
-- f1 ~ Maybe


-- Prelude> :t (fmap . fmap . fmap) replaceWithP
-- (fmap . fmap . fmap) replaceWithP
-- :: (Functor f2, Functor f1, Functor f) =>
-- f (f1 (f2 a)) -> f (f1 (f2 Char))
thriceLifted :: (Functor f2, Functor f1, Functor f) => f (f1 (f2 a)) -> f (f1 (f2 Char))
thriceLifted = (fmap . fmap . fmap) replaceWithP
-- More specific or "concrete"
thriceLifted' :: [Maybe [Char]] -> [Maybe [Char]]
thriceLifted' = thriceLifted
-- f ~ []
-- f1 ~ Maybe
-- f2 ~ []



a = fmap (+1) $ read "[1]" :: [Int]
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])
c = fmap (*2) (\x -> x - 2)
d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])
e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap (read . ("123"++) . show) ioi
    in fmap (*3) changed




functorCompose' :: (Eq (f c), Functor f) => f a
                                            -> Fun a b
                                            -> Fun b c
                                            -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
    (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool


functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

-----------------------------

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity x) = Identity $ f x

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        x <- arbitrary
        return $ Identity x

type IntIdentity = Identity Int -> IntToInt -> IntToInt -> Bool

-----------------------------

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
    fmap f (Pair x x') = Pair (f x) (f x')

instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary = do
        x <- arbitrary
        x' <- arbitrary
        return $ Pair x x'

type IntPair = Pair Int -> IntToInt -> IntToInt -> Bool

-----------------------------

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        x <- arbitrary
        x' <- arbitrary
        return $ Two x x'

type IntTwo = Two String Int -> IntToInt -> IntToInt -> Bool

-----------------------------

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        x <- arbitrary
        x' <- arbitrary
        x'' <- arbitrary
        return $ Three x x' x''

type IntThree = Three String Int Int -> IntToInt -> IntToInt -> Bool
-----------------------------

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' a b c) = Three' a (f b) (f c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = do
        x <- arbitrary
        x' <- arbitrary
        x'' <- arbitrary
        return $ Three' x x' x''

type IntThree' = Three' String Int -> IntToInt -> IntToInt -> Bool
-----------------------------

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = do
        x <- arbitrary
        x' <- arbitrary
        x'' <- arbitrary
        x''' <- arbitrary
        return $ Four x x' x'' x'''

type IntFour = Four String Int String Int -> IntToInt -> IntToInt -> Bool
-----------------------------

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
    fmap f (Four' a b c d) = Four' a b c (f d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = do
        x <- arbitrary
        x' <- arbitrary
        x'' <- arbitrary
        x''' <- arbitrary
        return $ Four' x x' x'' x'''

type IntFour' = Four' String Int -> IntToInt -> IntToInt -> Bool

----------------------

data Possibly a =
    LolNope
    | Yeppers a
    deriving (Eq, Show)

instance Functor Possibly where
    fmap _ LolNope = LolNope
    fmap f (Yeppers x) = Yeppers $ f x

------------------------

-- newtype Mu f = InF { outF :: f (Mu f) }
--
-- instance Functor Mu where
--   fmap = undefined

------------------

data Quant a b =
    Finance
    | Desk a
    | Bloor b deriving (Eq, Show)

instance Functor (Quant a) where
    fmap f (Bloor b) = Bloor $ f b
    fmap _ Finance = Finance
    fmap _ (Desk a) = Desk a

-----------------------

data K a b = K a deriving (Eq, Show)

instance Functor (K a) where
    fmap _ (K a) = K a

-----------------------


newtype Flip f a b =
    Flip (f b a)
    deriving (Eq, Show)

-- newtype K a b = K a

-- should remind you of an
-- instance you've written before
instance Functor (Flip K a) where
    fmap f (Flip (K a)) = Flip $ K $ f a

--------------------------

data EvilGoateeConst a b =
    GoatyConst b

instance Functor (EvilGoateeConst a) where
    fmap f (GoatyConst b) = GoatyConst $ f b

-------------------

data LiftItOut f a =
    LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
    fmap f (LiftItOut fa)  = LiftItOut $ fmap f fa

-------------------

data Parappa f g a =
    DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

----------------------

data IgnoreOne f g a b =
    IgnoringSomething (f a) (g b)

instance Functor g => Functor (IgnoreOne f g a) where
    fmap f (IgnoringSomething fa gb) = IgnoringSomething fa $ fmap f gb

----------------------

data Notorious g o a t =
    Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a) where
    fmap f (Notorious go ga gt) = Notorious go ga $ fmap f gt

---------------------

data List a =
    Nil
    | Cons a (List a)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons a b) = Cons (f a) $ fmap f b

------------------------

data GoatLord a =
    NoGoat
    | OneGoat a
    | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLord where
    fmap _ NoGoat = NoGoat
    fmap f (OneGoat a) = OneGoat (f a)
    fmap f (MoreGoats a b c) = MoreGoats (fmap f a) (fmap f b) (fmap f c)

-------------------------

data TalkToMe a =
    Halt
    | Print String a
    | Read (String -> a)

instance Functor TalkToMe where
    fmap _ Halt = Halt
    fmap f (Print s a) = Print s (f a)
    fmap f (Read g) =  Read $ fmap f g

main :: IO ()
main = do
    putStr "replaceWithP' lms: "
    print (replaceWithP' lms)
    putStr "liftedReplace lms: "
    print (liftedReplace lms)
    putStr "liftedReplace' lms: "
    print (liftedReplace' lms)
    putStr "twiceLifted lms: "
    print (twiceLifted lms)
    putStr "twiceLifted' lms: "
    print (twiceLifted' lms)
    putStr "thriceLifted lms: "
    print (thriceLifted lms)
    putStr "thriceLifted' lms: "
    print (thriceLifted' lms)

    quickCheck (functorCompose' :: IntFC)

    quickCheck (functorIdentity :: Identity Int -> Bool)
    quickCheck (functorCompose' :: IntIdentity)
    quickCheck (functorIdentity :: Pair Int -> Bool)
    quickCheck (functorCompose' :: IntPair)
    quickCheck (functorIdentity :: Two String Int -> Bool)
    quickCheck (functorCompose' :: IntTwo)
    quickCheck (functorIdentity :: Three String String Int -> Bool)
    quickCheck (functorCompose' :: IntThree)
    quickCheck (functorIdentity :: Three' String Int -> Bool)
    quickCheck (functorCompose' :: IntThree')
    quickCheck (functorIdentity :: Four String Int String Int -> Bool)
    quickCheck (functorCompose' :: IntFour)
    quickCheck (functorIdentity :: Four' String Int -> Bool)
    quickCheck (functorCompose' :: IntFour')
