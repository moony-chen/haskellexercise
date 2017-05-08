import Data.Char (digitToInt)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

--safeTail :: [a] -> Maybe [a]
--safeLast :: [a] -> Maybe a
--safeInit :: [a] -> Maybe [a]


splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith func (x:[]) 
        | func x = [[]]
        | otherwise = [[x]]
splitWith func (x:xs) 
        | func x = [] : result
        | otherwise = [(x: head result)] ++ (tail result)
    where result = splitWith func xs


asInt_fold :: String -> Int
asInt_fold xs = foldl step 0 xs
    where step a x = 10 * a + (digitToInt x)


suffixes :: [a] -> [[a]]
suffixes xs@(_:xs') = xs : suffixes xs'
suffixes _ = []