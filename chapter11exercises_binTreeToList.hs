module BinaryTreeToList where

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) =
  concat [[a], preorder left, preorder right]

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) =
  concat [inorder left, [a], inorder right]

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) =
  concat [postorder left, postorder right, [a]]

testTree :: BinaryTree Integer
testTree =
  Node (Node Leaf 1 Leaf)
       2
       (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2,1,3]
  then putStrLn "Preorder gucci!"
  else putStrLn "x.x - preorder"

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
  then putStrLn "Inorder fine!"
  else putStrLn "Bad news inorder bears."

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
    then putStrLn "Postorder fine!"
    else putStrLn "Ugh. postorder broken"

main :: IO ()
main = do
  testPreorder
  testInorder
  testPostorder
