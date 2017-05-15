module Chapter12 where

import Data.Char

-- example GHCi session above the functions
-- >>> notThe "the"
-- Nothing
-- >>> notThe "blahtheblah"
-- Just "blahtheblah"
-- >>> notThe "woot"
-- Just "woot"

notThe :: String -> Maybe String
notThe s
    | map toLower s == "the" = Nothing
    | otherwise = Just s

-- >>> replaceThe "the cow loves us"
-- "a cow loves us"
replaceThe :: String -> String
replaceThe  = unwords . (map (maybe2A . notThe)) . words  where
                            maybe2A Nothing = "a"
                            maybe2A (Just s) = s
                        

-- >>> countTheBeforeVowel "the cow"
-- 0
-- >>> countTheBeforeVowel "the evil cow"
-- 1
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = undefined


-- >>> countVowels "the cow"
-- 2
-- >>> countVowels "Mikolajczak"
-- 4
countVowels :: String -> Integer
countVowels = undefined


newtype Word' = Word' String deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord str = let    countVandC char (v0, c0)
                        | char `elem` vowels = (v0+1, c0)
                        | otherwise = (v0, c0+1)
                    (v, c) = foldr countVandC (0,0) str 
             in case v > c of True -> Nothing  
                              False -> Just (Word' str)  
               


-- As natural as any competitive bodybuilder
data Nat =
      Zero
    | Succ Nat
    deriving (Eq, Show)
-- >>> natToInteger Zero
-- 0    
-- >>> natToInteger (Succ Zero)
-- 1
-- >>> natToInteger (Succ (Succ Zero))
-- 2
natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = natToInteger n + 1 


-- >>> integerToNat 0
-- Just Zero
-- >>> integerToNat 1
-- Just (Succ Zero)
-- >>> integerToNat 2
-- Just (Succ (Succ Zero))
-- >>> integerToNat (-1)
-- Nothing
integerToNat :: Integer -> Maybe Nat
integerToNat n
    | n == 0 = Just Zero
    | n < 0 = Nothing
    | otherwise = Just (integerToNat' n) where
                        integerToNat' :: Integer -> Nat
                        integerToNat' 0 = Zero
                        integerToNat' n = Succ $ integerToNat' (n-1)



-- >>> isJust (Just 1)
-- True
-- >>> isJust Nothing
-- False
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False
-- >>> isNothing (Just 1)
-- False
-- >>> isNothing Nothing
-- True
isNothing :: Maybe a -> Bool
isNothing = not . isJust


-- >>> mayybee 0 (+1) Nothing
-- 0
-- >>> mayybee 0 (+1) (Just 1)
-- 2
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing = b
mayybee _ f (Just a) = f a


-- >>> fromMaybe 0 Nothing
-- 0
-- >>> fromMaybe 0 (Just 1)
-- 1
fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing = a
fromMaybe _ (Just a) = a

-- >>> listToMaybe [1, 2, 3]
-- Just 1
-- >>> listToMaybe []
-- Nothing
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x
-- >>> maybeToList (Just 1)
-- [1]
-- >>> maybeToList Nothing
-- []
maybeToList :: Maybe a -> [a]
maybeToList (Just a) = [a]
maybeToList Nothing = []


-- >>> catMaybes [Just 1, Nothing, Just 2]
-- [1, 2]
-- >>> catMaybes [Nothing, Nothing, Nothing]
-- []
catMaybes :: [Maybe a] -> [a]
catMaybes = map unboxJust . filter isJust where
                unboxJust (Just a) = a

catMaybes' :: [Maybe a] -> [a]
catMaybes' = foldr funbox []  where
                funbox (Just a) b = a : b
                funbox Nothing b = b


-- >>> flipMaybe [Just 1, Just 2, Just 3]
-- Just [1, 2, 3]
-- >>> flipMaybe [Just 1, Nothing, Just 3]
-- Nothing
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs 
    | hasNothing xs = Nothing 
    | otherwise = Just $ catMaybes xs where
        hasNothing [] = False
        hasNothing (Nothing:xs) = True
        hasNothing (Just x:xs) = hasNothing xs



lefts' :: [Either a b] -> [a]
lefts' = foldr funbox []  where
                funbox (Left a) b = a : b
                funbox (Right a) b = b 

rights' :: [Either a b] -> [b]
rights' = foldr funbox []  where
                funbox (Left a) b = b
                funbox (Right a) b = a : b 

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr f ([], []) where
                        f (Left a) (x, y) = (a:x, y)
                        f (Right a) (x, y) = (x, a:y)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right b) = Just $ f b

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a) = f a
either' _ h (Right b) = h b


eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f  = either' f' f'' where 
                    f' _ = Nothing
                    f'' = Just . f



myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)


myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b
    | isNothing (f b) = []
    | otherwise = let Just (a1, b1) = f b in
                    a1 : myUnfoldr f b1

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr g x where
                        g x = Just (x, (f x))