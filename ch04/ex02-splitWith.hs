-- Write a function splitWith that acts similarly to words, but takes a
-- predicate and a list of any type, and splits its input list on every element
-- for which the predicate returns False.
--

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith predicate list = 
    let (pre, suf) = break predicate list
    in pre : case suf of 
                (_ : rest) -> splitWith predicate rest
                _          -> []


-- An interesting solution taken from comments on the book's site
-- Uses only 'not' standard function
splitWith2 :: (a -> Bool) -> [a] -> [[a]]
splitWith2 _ [] = []
splitWith2 _ (x:[]) = [x] : []
splitWith2 predicate (x:xs)
    | not (predicate x)  = [x] : current : rest
    | otherwise     = (x : current) : rest
        where current : rest = splitWith predicate xs


isCharA c = c == 'a'
