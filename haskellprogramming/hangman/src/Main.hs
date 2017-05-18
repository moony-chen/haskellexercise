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
fillInCharacter (Puzzle word filledInSoFar s) c = Puzzle word filledIn s' where 
        filledIn = zipWith zipper filledInSoFar (map checkWord word) where 
                checkWord cinw 
                    | cinw == c = Just c
                    | otherwise = Nothing
                zipper a b
                    | isJust a = a
                    | isJust b = b
                    | otherwise = Nothing
        s' = c:s

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
    putStrLn $ "Your guess was: " ++ [guess]
    case (charInWord puzzle guess
        , alreadyGuessed puzzle guess) of
    (_, True) -> do
        putStrLn "You already guessed that\
        \ character, pick something else!"
        return puzzle
    (True, _) -> do
        putStrLn "This character was in the word,\
        \ filling in the word accordingly"
        return (fillInCharacter puzzle guess)
    (False, _) -> do
        putStrLn "This character wasn't in\
        \ the word, try again."
        return (fillInCharacter puzzle guess)


gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
    if (length guessed) > 7 then
        do putStrLn "You lose!"
            putStrLn $ "The word was: " ++ wordToGuess
            exitSuccess
    else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
    if all isJust filledInSoFar then
        do putStrLn "You win!"
            exitSuccess
    else return ()


runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
    gameOver puzzle
    gameWin puzzle
    putStrLn $ "Current puzzle is: " ++ show puzzle
    putStr "Guess a letter: "
    guess <- getLine
    case guess of
        [c] -> handleGuess puzzle c >>= runGame
        _ -> putStrLn "Your guess must\
                        \ be a single character"

main :: IO ()
main = do
    word <- randomWord'
    let puzzle = freshPuzzle (fmap toLower word)
    runGame puzzle
