module Chapter20 where

import Data.Monoid
import Data.Foldable

data Identity a = Identity a

instance Foldable Identity where
  foldr f z (Identity x) = f x z
  foldl f z (Identity x) = f z x
  foldMap f (Identity x) = f x

-- -------------------------------------
data Optional a =
    Nada
    | Yep a
    deriving (Eq, Show)

instance Foldable Optional where
  foldr _ z Nada = z
  foldr f z (Yep x) = f x z
  foldl _ z Nada = z
  foldl f z (Yep x) = f z x
  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a

sum' :: (Foldable t, Num a) => t a -> a
sum' = foldr (+) 0

sum'' :: (Foldable t, Num a) => t a -> a
sum'' xs = getSum $ foldMap Sum xs

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' e xs = getAny $ foldMap (Any . ( (==) e)) xs

newtype Min a = Min {getMin :: Maybe a} deriving (Eq, Show)

instance Ord a => Monoid (Min a) where
  mempty = Min Nothing
  mappend (Min Nothing) (Min Nothing) = Min Nothing
  mappend (Min Nothing) (Min (Just y)) = Min $ Just y
  mappend (Min (Just x)) (Min Nothing) = Min $ Just x
  mappend (Min (Just x)) (Min (Just y)) = Min $ Just $ min x y


minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' xs = getMin $ foldMap (Min . Just) xs
