module BinaryTree where

import Chapter12

data BinaryTree a =
      Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
    | b == a = Node left a right
    | b < a = Node (insert' b left) a right
    | b > a = Node left a (insert' b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
    Node (mapTree f left) (f a) (mapTree f right)


testTree' :: BinaryTree Integer
testTree' =
    Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)
mapExpected =
    Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)
-- acceptance test for mapTree
mapOkay =
    if mapTree (+1) testTree' == mapExpected
    then print "yup okay!"
    else error "test failed!"

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = a : (preorder left) ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = (inorder left) ++ [a] ++ (inorder right)

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = (postorder left) ++ (postorder right) ++ [a]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
    if preorder testTree == [2, 1, 3]
    then putStrLn "Preorder fine!"
    else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
    if inorder testTree == [1, 2, 3]
    then putStrLn "Inorder fine!"
    else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
    if postorder testTree == [1, 3, 2]
    then putStrLn "Postorder fine!"
    else putStrLn "postorder failed check"

main :: IO ()
main = do
    testPreorder
    testInorder
    testPostorder

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ b Leaf = b
foldTree f b (Node left a right) = foldTree f (foldTree f (f a b) left) right


mapTree' :: Ord b => (a -> b) -> BinaryTree a -> BinaryTree b
mapTree' f bt =
    foldTree (mapTreeH f) Leaf bt

mapTreeH :: Ord b => (a -> b) -> a -> BinaryTree b -> BinaryTree b
mapTreeH f a bt = insert' (f a) bt

-- mapTreeH f a Leaf = Node Leaf (f a) Leaf

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f a0 
    | isNothing (f a0) = Leaf
    | otherwise = let Just (a1, b0, a2) = f a0 in 
                        Node (unfold f a1) b0 (unfold f a2)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold (f n) 0 where
        f n i
            | n <= 0 = Nothing
            | i >= n = Nothing
            | otherwise = Just (i+1, i, i+1)