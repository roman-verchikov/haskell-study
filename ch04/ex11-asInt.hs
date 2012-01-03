--------------------------------------------------------------------------------
-- Use a fold (choosing the appropriate fold will make your code much simpler)
-- to rewrite and improve upon the asInt function from the section called
-- “Explicit recursion”.
--
--------------------------------------------------------------------------------
-- Your function should behave as follows.  
--------------------------------------------------------------------------------
-- ghci> asInt_fold "101"
-- 101
-- ghci> asInt_fold "-31337"
-- -31337
-- ghci> asInt_fold "1798"
-- 1798
--------------------------------------------------------------------------------
-- Extend your function to handle the following kinds of exceptional conditions
-- by calling error.
--------------------------------------------------------------------------------
-- ghci> asInt_fold ""
-- 0
-- ghci> asInt_fold "-"
-- 0
-- ghci> asInt_fold "-3"
-- -3
-- ghci> asInt_fold "2.7"
-- *** Exception: Char.digitToInt: not a digit '.'
-- ghci> asInt_fold "314159265358979323846"
-- 564616105916946374
--------------------------------------------------------------------------------
--
import Data.Char

asInt_fold :: String -> Int
asInt_fold ('-':rest) = -(asInt_fold rest)
asInt_fold s = foldr step 0 (reverse s)
    where step x acc = acc * 10 + (digitToInt x)
