-- | Define a function that joins a list of lists together using a separator
-- | value. The separator should appear between elements of the list, but should
-- | not follow the last element. Your function should behave as follows.
-- @
-- ghci> :load Intersperse
-- [1 of 1] Compiling Main             ( Intersperse.hs, interpreted )
-- Ok, modules loaded: Main.
-- ghci> intersperse ',' []
-- ""
-- ghci> intersperse ',' ["foo"]
-- "foo"
-- ghci> intersperse ',' ["foo","bar","baz","quux"]
-- "foo,bar,baz,quux"
-- @
-- |

intersperse :: a -> [[a]] -> [a]
intersperse _ []       = [] 
intersperse _ (x:[])   = x
intersperse sep (x:xs) = x ++ [sep] ++ intersperse sep xs
