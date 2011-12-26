-- | Create a function that sorts a list of lists based on the length of each sublist. 
-- | (You may want to look at the 'sortBy' function from the 'Data.List' module.)

import Data.List

sortListOfLists :: [[a]] -> [[a]]
sortListOfLists l = sortBy compareLength l
    where compareLength lhs rhs = compare (length lhs) (length rhs)
