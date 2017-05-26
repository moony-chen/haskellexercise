{-
stack --resolver lts-7.16 ghci --package hspec QuickCheck
:l CipherTest.hs Cipher.hs
-}

module CipherTest where

import Test.Hspec
import Test.QuickCheck
import Cipher(caesar, unCaesar, unVigenère, vigenère)

prop_caesarUnCaesar i str = unCaesar i (caesar i str) == str

prop_vigenèreUnVigenère key str = unVigenère key (vigenère key str) == str

main :: IO ()
main = hspec $ do
    describe "caesar" $ do
        it "caesar then unCaesar should have the original string " $ do
            quickCheck prop_caesarUnCaesar
    describe "vigenère" $ do
        it "vigenère then unVigenère should have the original string " $ do
            quickCheck prop_vigenèreUnVigenère
