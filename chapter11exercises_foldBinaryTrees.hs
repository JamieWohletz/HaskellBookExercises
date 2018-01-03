module FoldBinaryTrees where

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) =
  concat [inorder left, [a], inorder right]

testTree :: BinaryTree Int
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f b tree =
  foldr f b (inorder tree)

test :: IO ()
test =
  if foldTree (+) 0 testTree == 6
  then putStrLn "Noice!"
  else putStrLn "Uh oh."
