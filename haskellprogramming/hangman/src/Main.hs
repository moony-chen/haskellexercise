module Main where

import Data.Char (toLower) -- [2] 
import Hangman


main :: IO ()
main = do
    word <- randomWord'
    let puzzle = freshPuzzle (fmap toLower word)
    runGame puzzle
