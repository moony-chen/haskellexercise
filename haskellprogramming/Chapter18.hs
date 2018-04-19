module Chapter18 where

import Data.Monoid
import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x*x, x*x]
    else []


data Cow = Cow {
    name :: String
  , age :: Int
  , weight :: Int
  } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n
    | n >= 0 = Just n
    | otherwise = Nothing

-- if Cow's name is Bess, must be under 500
weightCheck :: Cow -> Maybe Cow
weightCheck c =
  let w = weight c
      n = name c
  in if n == "Bess" && w > 499
    then Nothing
    else Just c


mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name' age' weight' =
  case noEmpty name' of
    Nothing -> Nothing
    Just nammy ->
      case noNegative age' of
        Nothing -> Nothing
        Just agey ->
          case noNegative weight' of
            Nothing -> Nothing
            Just weighty ->
              weightCheck (Cow nammy agey weighty)


mkSphericalCow' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow' name' age' weight' = do
  nammy <- noEmpty name'
  agey <- noNegative age'
  weighty <- noNegative weight'
  weightCheck (Cow nammy agey weighty)

-----------18.7 Chapter Exercises

data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap f _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  return = pure
  _ >>= _ = NopeDotJpg

instance EqProp (Nope a) where
  a =-= b = eq a b

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

--------------

data PhhhbbtttEither b a =
    Left' a
  | Right' b
  deriving (Eq, Show)

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where
  a =-= b = eq a b

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither b a) where
  arbitrary =
    frequency [ (1, Left' <$> arbitrary)
              , (1, Right' <$> arbitrary) ]

instance Functor (PhhhbbtttEither b) where
  fmap f (Left' a) = Left' $ f a
  fmap _ (Right' b) = Right' b

instance Monoid b => Applicative (PhhhbbtttEither b) where
  pure a = Left' a
  Left' f <*> Left' v = Left' (f v)
  Left' f <*> Right' b = Right' b
  Right' b <*> Right' b' = Right' $ b <> b'
  Right' b <*> Left' a = Right' b

instance Monoid b => Monad (PhhhbbtttEither b) where
  return = pure
  Left' a >>= f = f a
  Right' b >>= f = Right' b

--------------------------------

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance (Eq a) => EqProp (Identity a) where
  a =-= b = eq a b

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  Identity f <*> (Identity a) = Identity $ f a

instance Monad Identity where
  return = pure
  Identity a >>= f = f a

-----------------------

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys
--
fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

-- write this one in terms of concat' and fmap
flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fmap f as

take' :: Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' n (Cons a as) = Cons a (take' (n - 1) as)

-- http://codingstruggles.com/haskell/arbitrary-length-lists-quickcheck.html
arbitraryList :: Arbitrary a => Int -> Gen (List a)
arbitraryList m
    | m == 0 = return Nil
    | m > 6 = arbitraryList 6
    | otherwise = Cons <$> arbitrary <*> arbitraryList (m-1)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = sized arbitraryList

instance Eq a => EqProp (List a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = take' 3000 xs
          ys' = take' 3000 ys

instance Functor List
  where fmap _ Nil = Nil
        fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Applicative List
  where
    pure a = Cons a Nil
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    Cons f fs <*> vs = fmap f vs `append` (fs <*> vs)

instance Monad List where
  return = pure
  Nil >>= f = Nil
  Cons a as >>= f = f a `append` (as >>= f)

j :: Monad m => m (m a) -> m a
j m = m >>= id
-- j m'' = m'' >>= \m' -> m' >>= \m -> return m
-- j m'' = do
--   m' <- m''
--   m <- m'
--   return m

main :: IO ()
main = do
  quickBatch (functor (undefined :: Nope (Int, String, String)))
  quickBatch (applicative (undefined :: Nope (Int, String, String)))
  quickBatch (monad (undefined :: Nope (Int, String, String)))
  quickBatch (functor (undefined :: PhhhbbtttEither String (Int, String, String)))
  quickBatch (applicative (undefined :: PhhhbbtttEither String (Int, String, String)))
  quickBatch (monad (undefined :: PhhhbbtttEither String (Int, String, String)))
  quickBatch (functor (undefined :: Identity (Int, String, String)))
  quickBatch (applicative (undefined :: Identity (Int, String, String)))
  quickBatch (monad (undefined :: Identity (Int, String, String)))

  quickBatch (functor (undefined :: List (Int, String, String)))
  quickBatch (applicative (undefined :: List (Int, String, String)))
  quickBatch (monad (undefined :: List (Int, String, String)))
