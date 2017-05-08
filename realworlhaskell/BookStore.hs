-- file: ch03/BookStore.hs
data BookInfo = Book Int String [String]
                deriving (Show)
myInfo = Book 9780135072455 "Algebra of Programming"
         ["Richard Bird", "Oege de Moor"]      
                   
data MagazineInfo = Magazine Int String [String]
                    deriving (Show)


type CustomerID = Int
type ReviewBody = String
type Address = [String]

data BetterReview = BetterReview BookInfo CustomerID ReviewBody


-- file: ch03/add.hs
sumList (x:xs) = x + sumList xs
sumList []     = 0


data Customer = Customer {
      customerID      :: CustomerID
    , customerName    :: String
    , customerAddress :: Address
    } deriving (Show)

data List a = Cons a (List a)
            | Nil
              deriving (Show)

toList (x:xs) = Cons x (toList xs)
toList [] = Nil

fromList (Cons x (y)) = x:(fromList y)
fromList Nil = []

data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

mylen::[a]->Int
mylen [] = 0
mylen (x:xs) =  1 + (mylen xs)

mySum [] = 0
mySum (x:xs) = x + (mySum xs)

myMean [] = 0
myMean (xs) = (mySum xs) / fromIntegral (mylen xs)

toPalindrome (xs) = xs ++ (myReverse xs)
    where myReverse (x:xs) = (myReverse xs) ++ [x]
          myReverse [] = []

isPalindrome [] = True
isPalindrome (xs)
    | length xs == 1 = True
isPalindrome (x:xs) = (x==last xs) && (isPalindrome (init xs))


intersperse _ [] = []
intersperse _ (x:[]) = x
intersperse s (x:xs) = x ++ [s] ++ (intersperse s xs)


heightOfTree Empty = 0
heightOfTree (Node a (b) (c)) = 1 + max (heightOfTree b) (heightOfTree c)