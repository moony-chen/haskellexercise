module Addition where

import Test.Hspec
import Test.QuickCheck


dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0 where
     go n d count
        | n < d = (count, n)
        | otherwise = go (n - d) d (count + 1)

summation :: (Ord a, Num a) => a -> a -> a
summation a1 a2 
    | a1 <= 0 = 0
    | otherwise = a2 + summation (a1 - 1) a2

trivialInt :: Gen Int
trivialInt = return 1

oneThroughThree :: Gen Int
oneThroughThree = elements [1, 2, 3]

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
    a <- arbitrary
    b <- arbitrary
    return (a, b)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genThreeple = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (a, b, c)

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
    a <- arbitrary
    b <- arbitrary
    elements [Left a, Right b]
-- equal probability

genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
    a <- arbitrary
    elements [Nothing, Just a]
-- What QuickCheck actually does
-- so you get more Just values
genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
    a <- arbitrary
    frequency [ (1, return Nothing), (3, return (Just a))]
-- frequency :: [(Int, Gen a)] -> Gen a


prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater

main :: IO ()
main = hspec $ do
    describe "Addition" $ do
        it "1 + 1 is greater than 1" $ do
            (1 + 1) > 1 `shouldBe` True
        it "2 + 2 is equal to 4" $ do
            2 + 2 `shouldBe` 4
        it "x + 1 is always greater than x" $ do
            property $ \x -> x + 1 > (x :: Int)
    describe "Division" $ do
        it "15 divided by 3 is 5" $ do
            dividedBy 15 3 `shouldBe` (5, 0)
        it "22 divided by 5 is 4 remainder 2" $ do
            dividedBy 22 5 `shouldBe` (4, 2)
    describe "Multiply by summation" $ do 
        it " 3 * 4 == 12 " $ do
            summation 3 4 `shouldBe` 12
