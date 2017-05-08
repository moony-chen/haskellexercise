module Water where

import Data.List

--http://chrisdone.com/posts/twitter-problem-loeb

water :: [Int] -> Int
water [] = 0
water ws = sum $ map (waterCount . flip walls ws) $ levels ws

levels :: [Int] -> [Int]
levels xs = 
    let maxi = maximum xs
        mini = minimum xs 
    in  [mini..maxi] 

walls :: Int -> [Int] -> [Bool]
walls x = map (>=x)

spaceCount :: [Bool] -> Int
spaceCount bs = 
    let indices = elemIndices True bs
    in  maximum indices - minimum indices +1

wallCount :: [Bool] -> Int
wallCount = length . filter id 

waterCount :: [Bool] -> Int
waterCount bs = spaceCount bs - wallCount bs

