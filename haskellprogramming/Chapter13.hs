module Chapter13 where

import Control.Monad
import System.Exit (exitSuccess) 
import Data.Char
import System.IO

palindrome :: IO () 
palindrome = forever $ do
    line1 <- getLine
    case (line1 == reverse line1) of
        True -> putStrLn "It's a palindrome!" 
        False -> exitSuccess

palindrome' :: IO () 
palindrome' = forever $ do
    line1 <- getLine
    let line2 = filter isLower (map toLower line1) in 
        case (line2 == reverse line2) of
            True -> putStrLn "It's a palindrome!" 
            False -> exitSuccess
        

type Name = String
type Age = Integer
     
data Person = Person Name Age deriving Show
     
data PersonInvalid = NameEmpty
                    | AgeTooLow
                    | PersonInvalidUnknown String 
                    deriving (Eq, Show)
mkPerson :: Name
            -> Age
            -> Either PersonInvalid Person 
mkPerson name age
    | name /= "" && age > 0 = Right $ Person name age 
    | name == "" = Left NameEmpty
    | not (age > 0) = Left AgeTooLow
    | otherwise = Left $ PersonInvalidUnknown $
        "Name was: " ++ show name ++ " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
    hSetBuffering stdout NoBuffering 
    putStr "Please input name: "
    name <- getLine
    putStr "Please input age: "
    age' <- getLine
    let age = read age' :: Integer  
        ep = mkPerson name age in
        putStrLn $ showPerson ep where 
            showPerson (Right p) = "Yay! Successfully got a person: " ++ show p
            showPerson (Left pi) = show pi










