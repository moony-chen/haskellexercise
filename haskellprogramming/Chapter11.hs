module Chapter11 where

import Data.Char

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf sub seq = (filter (`elem` sub) seq) == sub


capitalizeWords :: String -> [(String, String)]
capitalizeWords str = map tupleCap $ words str where
                        tupleCap w = (w, capitalizeWord w)

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord w@(x:xs) = (toUpper x):xs


--Hutton’s Razor
data Expr
    = Lit Integer
    | Add Expr Expr

eval :: Expr -> Integer
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2

printExpr :: Expr -> String
printExpr (Lit n) = show n
printExpr (Add e1 e2) = printExpr e1 ++ " + " ++ printExpr e2