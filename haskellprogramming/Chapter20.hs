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
