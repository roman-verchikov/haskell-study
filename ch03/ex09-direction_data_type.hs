-- Consider three two-dimensional points a, b, and c. If we look at the angle
-- formed by the line segment from a to b and the line segment from b to c, it
-- either turns left, turns right, or forms a straight line. Define a Direction
-- data type that lets you represent these possibilities.
--

data Point = Point {
                 xOf :: Double,
                 yOf :: Double
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


getSegmentDirection :: Segment -> Direction
getSegmentDirection s 
    | angleBetween (line1 s) (line2 s) > 0 = TurnsLeft
    | angleBetween (line1 s) (line2 s) < 0 = TurnsRight
    | otherwise                            = StraightLine
        where line1 s = (Line (pt1 s) (pt2 s))
              line2 s = (Line (pt2 s) (pt3 s))
              angleBetween l1 l2 = 
