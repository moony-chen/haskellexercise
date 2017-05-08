
module Print1 where

import Data.Char

main :: IO ()
main = putStrLn "hello world!"

id1 :: a -> a -> a
id1 x y = x

id2 :: a -> a -> a
id2 x y = y

id3 x = id


f :: Int -> String
f = undefined
g :: String -> Char
g = undefined
h :: Int -> Char
h = g . f

munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge f1 f2 = fst . f2 . f1 

mc91 :: Integral a => a -> a
mc91 n
    | n > 100 = n - 10
    | otherwise = mc91 . mc91 $ n + 11


zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (a:as) (b:bs) = (a, b) : zip' as bs

zipWith' :: (a -> b-> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (a:as) (b:bs) = f a b : zipWith' f as bs

upperFirst :: String -> String
upperFirst [] = []
upperFirst (x:xs) = toUpper x : xs


myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f x || myAny f xs


myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem a (x:xs) = a == x || myElem a xs


myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ x:[]

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . map f

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f ax = myMaximumBy' f (head ax) ax
   where 
    myMaximumBy' f m [] = m
    myMaximumBy' f m (x:xs) 
        | f m x == GT = myMaximumBy' f m xs
        | otherwise = myMaximumBy' f x xs

seekritFunc x =
    (/) (sum (map (toFractional . length) (words x)))
        (toFractional $ length (words x))
    where toFractional = fromRational . toRational
