-- Using the binary tree type that we defined earlier in this chapter, write a
-- function that will determine the height of the tree. The height is the
-- largest number of hops from the root to an Empty. 
--
-- For example, the tree Empty has height zero; Node "x" Empty Empty has height
-- one; Node "x" Empty (Node "y" Empty Empty) has height two; and so on.

data BTree a = Node a (BTree a) (BTree a)
             | Empty
               deriving (Show)

binaryTreeHeight :: BTree a -> Int
binaryTreeHeight Empty = 0
binaryTreeHeight (Node _ left right)  = 1 + maxOf (binaryTreeHeight left) (binaryTreeHeight right)
    where maxOf lhs rhs 
            | lhs > rhs = lhs
            | otherwise = rhs
