module Chapter17 where

import Control.Applicative
import Data.Monoid
import Data.List (elemIndex)

-- added = (+3) (lookup 3 $ zip [1, 2, 3] [4, 5, 6])
added :: Maybe Integer
added = fmap (+3) (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

------------------------
y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

-------------------
maxed :: Maybe Int
maxed = max' <$> x <*> y where 
    x :: Maybe Int
    x = elemIndex 3 [1, 2, 3, 4, 5]
    y :: Maybe Int
    y = elemIndex 4 [1, 2, 3, 4, 5]
    max' :: Int -> Int -> Int
    max' = max

-----------------------

summed :: Maybe Integer
summed = fmap sum $ (,) <$> x <*> y where 
    xs = [1, 2, 3]
    ys = [4, 5, 6]
    x :: Maybe Integer
    x = lookup 3 $ zip xs ys
    y :: Maybe Integer
    y = lookup 2 $ zip xs ys

-------------------------

newtype Identity a = Identity a
    deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
    pure = Identity
    Identity f <*> Identity a = Identity $ f a 

-----------------------

newtype Constant a b =
    Constant { getConstant :: a }
    deriving (Eq, Ord, Show)

instance Functor (Constant a) where
    fmap f (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
    pure _ = Constant mempty
    Constant a <*> Constant b = Constant (a <> b)

--------------------------

-- 1. const <$> Just "Hello" <*> "World"
-- const <$> Just "Hello" <*> pure "World"

-- (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]