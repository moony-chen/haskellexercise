{-# LANGUAGE NoMonomorphismRestriction #-}

module DetermineTheType where

-- simple example\
example = 1


x = 5
y = x + 5

w = y * 10
-- z :: Num a => a -> a
z y = y * 10

-- f :: Fractional a => a
f = 4 / y

bigNum = (^) 5 $ 10
-- wahoo = bigNum $ 10

functionH :: [a] -> a
functionH (x:_) = x

functionC :: (Ord a) => a -> a -> Bool
functionC x y = if (x > y) then True else False

functionS :: (a, b) -> b
functionS (x, y) = y

a :: (a -> c) -> a -> a
a f a = a

a' :: (a -> b) -> a -> b
a' f a = f a

main :: IO ()
main = do
  print (1 + 2)
  putStrLn "10"
  print (negate (-1))
  print ((+) 0 blah)
    where blah = negate 1

munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge xy y2wz x = fst $ y2wz $ xy x
