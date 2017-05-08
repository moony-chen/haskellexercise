module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"

digits :: Int -> [Int]
digits n 
    | n < 10 = n:[]
    | otherwise = (digits . (`div` 10) $ n) ++ (n `mod` 10) : []

wordNumber :: Int -> String
wordNumber = concat . intersperse "-" . map digitToWord . digits


myWords :: String -> [String]
myWords [] = []
myWords xs = (takeWhile (/=' ') xs) : (myWords . dropWhile (==' ') . dropWhile (/=' ') $ xs)


data FlowerType = Gardenia
                | Daisy
                | Rose
                | Lilac
                deriving Show
type Gardener = String

data Garden =
    Garden Gardener FlowerType
    deriving Show    



data OperatingSystem =
          GnuPlusLinux
        | OpenBSDPlusNevermindJustBSDStill
        | Mac
        | Windows
        deriving (Eq, Show)
data ProgrammingLanguage =
          Haskell
        | Agda
        | Idris
        | PureScript
        deriving (Eq, Show)

data Programmer =
    Programmer { os :: OperatingSystem
               , lang :: ProgrammingLanguage }  
    deriving (Eq, Show)


allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
        [ GnuPlusLinux
        , OpenBSDPlusNevermindJustBSDStill
        , Mac
        , Windows
        ]
allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [Programmer {os = x, lang = y} | x <- allOperatingSystems, y <- allLanguages]