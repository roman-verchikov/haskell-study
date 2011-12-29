-- Consider three two-dimensional points a, b, and c. If we look at the angle
-- formed by the line segment from a to b and the line segment from b to c, it
-- either turns left, turns right, or forms a straight line. Define a Direction
-- data type that lets you represent these possibilities.
--
data Direction = TurnsLeft 
               | TurnsRight
               | StraightLine 
                 deriving (Show)
--------------------------------------------------------------------------------
-- Write a function that calculates the turn made by three 2D points and
-- returns a Direction.
getDirection a b c
    | lineEquation a b c  > 0 = TurnsLeft
    | lineEquation a b c  < 0 = TurnsRight
    | lineEquation a b c == 0 = StraightLine
        where lineEquation (x1,y1) (x2,y2) (x3,y3) = (y3-y1)*(x2-x1) - (y2-y1)*(x3-x1)

--------------------------------------------------------------------------------
-- Define a function that takes a list of 2D points and computes the direction
-- of each successive triple. Given a list of points [a,b,c,d,e], it should
-- begin by computing the turn made by [a,b,c], then the turn made by [b,c,d],
-- then [c,d,e]. Your function should return a list of Direction.
getDirections [] = []
getDirections (a:b:[]) = []
getDirections (a:b:c:xs) = (getDirection a b c : getDirections (b:c:xs))
