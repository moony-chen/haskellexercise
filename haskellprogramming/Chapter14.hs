module Main where

import Test.Hspec
import Test.QuickCheck

import Data.List (sort)
import Data.Char

-- for a function
half x = x / 2
-- this property should hold
halfIdentity = (*2) . half

-- for any list you apply sort to
-- this property should hold
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
    snd $ foldr go (Nothing, True) xs where 
        go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)

plusAssociative :: Int -> Int -> Int -> Bool
plusAssociative x y z =
    x + (y + z) == (x + y) + z

plusCommutative :: Int -> Int -> Bool
plusCommutative x y =
    x + y == y + x

multiplyAssociative :: Integer -> Integer -> Integer -> Bool
multiplyAssociative x y z =
    x * (y * z) == (x * y) * z

multiplyCommutative :: Integer -> Integer -> Bool
multiplyCommutative x y =
    x * y == y * x

powerAssociative :: Integer -> Integer -> Integer -> Bool
powerAssociative x y z =
    x ^ (y ^ z) == (x ^ y) ^ z

powerCommutative :: Integer -> Integer -> Bool
powerCommutative x y =
    x ^ y == y ^ x

quotRemLaw :: (Integer, Integer) -> Bool
quotRemLaw (x, y) = (quot x y)*y + (rem x y) == x

divModLaw :: (Integer, Integer) -> Bool 
divModLaw (x, y) = (div x y)*y + (mod x y) == x

nonZeroGen :: Gen Integer
nonZeroGen = suchThat arbitrary (\x -> x /= 0) 

genTuple :: Gen (Integer, Integer)
genTuple = do
    a <- arbitrary
    b <- nonZeroGen
    return (a, b)

{-prop_dollar :: (a -> b) -> a -> Bool
prop_dollar f a = (f $ a) == f a-}

prop_foldr1 :: [Int] -> [Int] -> Bool
prop_foldr1 x y = foldr (:) x y == (++) x y

prop_foldr2 :: [[Int]] -> Bool
prop_foldr2 x = foldr (++) [] x == concat x


takeLen n xs = length (take n xs) == n

readShow :: (Read a, Show a, Eq a) => a -> Bool
readShow x = (read (show x)) == x

square x = x * x
squareIdentity :: Double -> Bool
squareIdentity x = (square . sqrt) x == x


capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord w@(x:xs) = (toUpper x):xs

twice f = f . f
fourTimes = twice . twice   

idempotence1 x = capitalizeWord x == twice capitalizeWord x
idempotence1' x = capitalizeWord x == fourTimes capitalizeWord x
idempotence2 x = sort x == twice sort x
idempotence2' x = sort x == fourTimes sort x



data Fool =
    Fulse
    | Frue
    deriving (Eq, Show)

foolGen :: Gen Fool
foolGen = elements [Fulse, Frue]

foolGen' :: Gen Fool
foolGen' = frequency [(2, return Fulse), (1, return Frue)]

main :: IO ()
main = hspec $ do
    describe "halfIdentity" $ do
        it "x is always equal to halfIdentity x" $ do
            property $ \x -> halfIdentity x == (x :: Double)
    describe "ordered list" $ do
        it "a sorted list will return true for listOrdered" $ do
            property $ \xs -> listOrdered (sort (xs:: [Int])) == True
    describe "plus" $ do
        it "plus has associative" $ do
            quickCheck plusAssociative
        it "plus has commutative" $ do
            quickCheck plusCommutative
    describe "multiply" $ do
        it "multiply has associative" $ do
            quickCheck multiplyAssociative
        it "multiply has commutative" $ do
            quickCheck multiplyCommutative
{-    describe "power" $ do
        it "power has associative" $ do
            quickCheck powerAssociative
        it "power has commutative" $ do
            quickCheck powerCommutative-}
    describe "divide" $ do
        it "quotRemLaw" $ do
            quickCheck $ forAll genTuple quotRemLaw
        it "divModLaw" $ do
            quickCheck $ forAll genTuple divModLaw
    describe "list" $ do
        it "reverse twice will be same as original" $ do
            quickCheck $ \x -> (reverse . reverse) x == id (x :: String)
    -- 8. Write a property for the definition of ($). TODO
{-    describe "$ and ." $ do
        it "f $ a = f a" $ do
            quickCheck $ -}
    -- 9. See if these two functions are equal:
    describe "foldr" $ do
        it "foldr (:) == (++) " $ do
            quickCheck prop_foldr1
        it "foldr (++) [] == concat " $ do
            quickCheck prop_foldr2
    describe "readShow" $ do
        it "(read (show x)) == x" $ do
            quickCheck (readShow :: Int -> Bool)
            quickCheck (readShow :: String -> Bool)
        it "length (take n xs) == n" $ do
            quickCheck (takeLen :: Int -> String -> Bool)
        it "squareIdentity" $ do
            quickCheck squareIdentity
    describe "Idempotence" $ do
        it "1" $ do
            quickCheck idempotence1
            quickCheck idempotence1'
        it "2" $ do
            quickCheck (idempotence2 :: [Int] -> Bool)
            quickCheck (idempotence2' :: [Int] -> Bool)


