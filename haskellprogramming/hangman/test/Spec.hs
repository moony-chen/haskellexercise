module Main where

import Hangman
import Test.Hspec
import Test.QuickCheck


main :: IO ()
main = hspec $ do
    describe "fillInCharacter" $ do
        it "fill in char if present" $ do
            fillInCharacter (Puzzle "test" [Nothing, Nothing, Nothing, Nothing] "") 'e' 
                `shouldBe` (Puzzle "test" [Nothing, Just 'e', Nothing, Nothing] "e")
        it "not fill in if not present" $ do
            fillInCharacter (Puzzle "test" [Nothing, Nothing, Nothing, Nothing] "") 'g' 
                `shouldBe` (Puzzle "test" [Nothing, Nothing, Nothing, Nothing] "g")
    describe "handleGuess" $ do
        it "do nothing if already guessed" $ do
            puzzle' <- handleGuess (Puzzle "test" [Nothing, Nothing, Nothing, Nothing] "g") 'g' 
            puzzle' `shouldBe` (Puzzle "test" [Nothing, Nothing, Nothing, Nothing] "g")
        it "fillInCharacter if character in the word" $ do
            puzzle' <- handleGuess (Puzzle "test" [Nothing, Nothing, Nothing, Nothing] "g") 'e' 
            puzzle' `shouldBe` (Puzzle "test" [Nothing, Just 'e', Nothing, Nothing] "eg")
        it "log char in history if not in the word" $ do
            puzzle' <- handleGuess (Puzzle "test" [Nothing, Nothing, Nothing, Nothing] "g") 'h' 
            puzzle' `shouldBe` (Puzzle "test" [Nothing, Nothing, Nothing, Nothing] "hg")
