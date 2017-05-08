module Cipher where

import Data.Char

movChar :: Int -> Char -> Char
movChar i c 
    | c `elem` ['a'..'z'] = chr (mod (ord c - ord 'a' + i) 26 + ord 'a')
    | c `elem` ['A'..'Z'] = chr (mod (ord c - ord 'A' + i) 26 + ord 'A')
    | otherwise = c

caesar :: Int -> String -> String
caesar _ [] = []
caesar i s = map (movChar i) s

unCaesar :: Int -> String -> String
unCaesar s bx = caesar (negate s) bx

distance :: Char -> Char -> Int
distance a b = ord b - ord a

vigenère :: String -> String -> String
vigenère [] words = words
vigenère [a] words = words
vigenère key words = let distance0 = head key
                         numbering = tail $ scanl vcount 0 words 
                         mappedtokey = zipWith zipper words numbering where
                            zipper :: Char -> Int -> Char
                            zipper c n  
                                | alphabet c = key !! mod (n - 1) (length key) 
                                | otherwise = c  
                         mappedpos = map (distance distance0) mappedtokey
                     in zipWith movChar mappedpos words 

alphabet :: Char -> Bool
alphabet c = c `elem` ['a'..'z'] || c `elem` ['A'..'Z']

vcount :: Int -> Char -> Int
vcount o c
    | alphabet c = o + 1
    | otherwise = o