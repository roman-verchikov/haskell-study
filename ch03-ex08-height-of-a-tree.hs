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
binaryTreeHeight (Node _ left right)  = maxOf (dfs left 1) (dfs right 1)
    where dfs (Node _ l r) i          = maxOf (dfs l (i+1)) (dfs r (i+1))
          dfs Empty i  = i
          maxOf lhs rhs 
            | lhs > rhs = lhs
            | otherwise = rhs
