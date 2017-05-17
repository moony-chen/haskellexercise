module Main where

import Control.Monad (forever) -- [1] 
import Data.Char (toLower) -- [2] 
import Data.Maybe (isJust) -- [3] 
import Data.List (intersperse) -- [4] 
import System.Exit (exitSuccess) -- [5] 
import System.Random (randomRIO) -- [6]



type WordList = [String]

allWords :: IO WordList
allWords = do
    dict <- readFile "data/dict.txt" 
    return (lines dict)

minWordLength :: Int 
minWordLength = 5

maxWordLength :: Int 
maxWordLength = 9


gameWords :: IO WordList
gameWords = do
    aw <- allWords
    return (filter gameLength aw) 
    where gameLength w =
    let l = length (w :: String)
    in l > minWordLength && l < maxWordLength


randomWord :: WordList -> IO String 
randomWord wl = do
    randomIndex <- randomRIO (0 , length gameWords - 1 ) -- ^^^ you need to fill this part in
    return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord


data Puzzle = Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
    show (Puzzle _ discovered guessed) =
        (intersperse ' ' $ fmap renderPuzzleChar discovered) ++ " Guessed so far: " ++ guessed

renderPuzzleChar :: Maybe Char -> Char 
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

freshPuzzle :: String -> Puzzle 
freshPuzzle r = Puzzle r (map (const Nothing) r) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle r _ _) c = c `elem` r

alreadyGuessed :: Puzzle -> Char -> Bool 
alreadyGuessed (Puzzle _ _ g) c = c `elem` g

fillInCharacter :: Puzzle -> Char -> Puzzle 
fillInCharacter (Puzzle word filledInSoFar s) c =

main :: IO ()
main = do
  putStrLn "hello world"
