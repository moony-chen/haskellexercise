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
elem' e xs = getAny $ foldMap (Any . (e ==)) xs

newtype Min a = Min {getMin :: Maybe a} deriving (Eq, Show)

instance Ord a => Monoid (Min a) where
  mempty = Min Nothing
  mappend (Min Nothing) (Min Nothing) = Min Nothing
  mappend (Min Nothing) (Min (Just y)) = Min $ Just y
  mappend (Min (Just x)) (Min Nothing) = Min $ Just x
  mappend (Min (Just x)) (Min (Just y)) = Min $ Just $ min x y


minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' xs = getMin $ foldMap (Min . Just) xs

null' :: (Foldable t) => t a -> Bool
null' xs = isNothing $ getFirst $ foldMap (First . Just) xs
  where isNothing Nothing = True
        isNothing (Just _) = False

length' :: (Foldable t) => t a -> Int
length' xs = getSum $ foldMap (const (Sum 1)) xs

toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) []

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (mappend . f) mempty


-- 20.6 Chapter Exercises

data Constant a b = Constant a

instance Foldable (Constant a) where
  foldr _ z _ = z

data Two a b = Two a b

instance Foldable (Two a) where
  foldr f z (Two _ b) = f b z

data Three a b c = Three a b c

instance Foldable (Three a b) where
  foldr f z (Three _ _ c) = f c z

data Three' a b = Three' a b b

instance Foldable (Three' a) where
  foldMap f (Three' _ b b') = mappend (f b) (f b')

data Four' a b = Four' a b b b

instance Foldable (Four' a) where
  foldMap f (Four' _ a b c) = f a <> f b <> f c

-- Write a filter function for Foldable types using foldMap
filterF :: (Applicative f, Foldable t, Monoid (f a))
        => (a -> Bool) -> t a -> f a
filterF p = foldMap toM
  where toM x -- = if p x then pure x else mempty
         | p x = pure x
         | otherwise = mempty
