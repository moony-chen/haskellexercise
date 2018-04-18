module Chapter17 where

import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers 
import Test.QuickCheck.Classes


data List a = 
      Nil
    | Cons a (List a) deriving (Eq, Show)


instance Functor List where 
    fmap _ Nil = Nil
    fmap f (Cons a as) = Cons (f a) (fmap f as)


append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b 
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a 
concat' = fold append Nil

-- write this one in terms of concat' and fmap
flatMap :: (a -> List b) -> List a -> List b 
flatMap f as = concat' $ fmap f as

instance Eq a => EqProp (List a) where 
    (=-=) = eq

instance Arbitrary a => Arbitrary (List a) where
    func = 

instance Applicative List where 
    pure a = Cons a Nil
    Nil <*> _ = Nil 
    _ <*> Nil = Nil 
    Cons f fs <*> as = fmap f as `append` (fs <*> as)


main :: IO ()
main = 
    quickBatch $ applicative (undefined :: List (String, String, Int))












