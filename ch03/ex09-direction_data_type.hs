-- Consider three two-dimensional points a, b, and c. If we look at the angle
-- formed by the line segment from a to b and the line segment from b to c, it
-- either turns left, turns right, or forms a straight line. Define a Direction
-- data type that lets you represent these possibilities.
--

data Point = Point {
                 x :: Double,
                 y :: Double
             } deriving (Show)

data Line = Line Point Point
            deriving (Show)

data Segment = LineSegment {
                   pt1 :: Point,
                   pt2 :: Point,
                   pt3 :: Point
               } deriving (Show)

data Direction = TurnsLeft 
               | TurnsRight
               | StraightLine 
                 deriving (Show)
--------------------------------------------------------------------------------
-- Write a function that calculates the turn made by three 2D points and
-- returns a Direction.
--
getSegmentDirection :: Segment -> Direction
getSegmentDirection s 
    | lineEquation (pt1 s) (pt2 s) (pt3 s) > 0 = TurnsLeft
    | lineEquation (pt1 s) (pt2 s) (pt3 s) < 0 = TurnsRight
    | otherwise                                = StraightLine
        where lineEquation p1 p2 p3 = ((y p3) - (y p1)) * ((x p2) - (x p1)) - ( (y p2) - (y p1) ) * ( (x p3) - (x p1) )
--                                    (  y    -   y1  )   (  x2   -   x1  ) - (    y2  -   y1   )   (   x    -   x1   )



--------------------------------------------------------------------------------
-- Define a function that takes a list of 2D points and computes the direction
-- of each successive triple. Given a list of points [a,b,c,d,e], it should
-- begin by computing the turn made by [a,b,c], then the turn made by [b,c,d],
-- then [c,d,e]. Your function should return a list of Direction.
