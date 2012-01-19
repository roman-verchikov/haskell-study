-- Write your own definition of the standard takeWhile function, first using
-- explicit recursion, then foldr.

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile p (x:xs)
    | p x = x : (myTakeWhile p xs)
    | otherwise = []

myTakeWhile2 :: (a -> Bool) -> [a] -> [a]
myTakeWhile2 p xs = foldr step [] xs
    where step x ys
            | p x = x : ys
            | otherwise = []
