import Data.List
import Debug.Trace
data Direction = Leftish | Straight | Rightish deriving (Show, Eq)

type Point = (Float, Float)
type Vector = (Float, Float)

vectorDifference :: Point -> Point -> Vector
vectorDifference (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)

cross :: Vector -> Vector -> Float
cross (a, b) (u, v) = a * v - b * u

dirTurn :: Point -> Point -> Point -> Direction
dirTurn a b c = 
    case compare (cross u v) 0 of
        GT -> Leftish
        EQ -> Straight
        LT -> Rightish
        where u = vectorDifference a b 
              v = vectorDifference b c
        
listDirTurn :: [Point] -> [Direction]
listDirTurn (x:y:z:zs) = (dirTurn x y z) : (listDirTurn (y:z:zs))
listDirTurn _ = []


-- Returns the convex hull of a set of points
grahamScan :: [Point] -> [Point]
grahamScan ps = grahamIter [] (grahamSort ps)

grahamSort ps = x : sortPointsByAngle x xs
    where (x:xs) = sortPoints ps

grahamIter :: [Point] -> [Point] -> [Point]
grahamIter [] (x:y:z:zs) =  grahamIter ((\s -> trace ("Basic stack: " ++ show s) s) [z, y, x]) zs
grahamIter [] _ = error "Not enough points to process. Or something."
grahamIter stack [] = stack
grahamIter stack@(t:n:ss) (x:xs)
    | dirTurn n t x /= Rightish = do grahamIter (trace ("Lost one: " ++ show t) (n:ss)) 
                                               (trace ("Still at pi = " ++ show x) (x:xs))
    | dirTurn n t x == Rightish = grahamIter (x:stack) xs
        
sneakyPrint :: (Show a) => a -> a
sneakyPrint x = trace (show x) x 

sortPoints :: [Point] -> [Point]
sortPoints = sortBy ordPoints 
    where ordPoints :: Point -> Point -> Ordering
          ordPoints (x, y) (z, w) = compare x z

sortPointsByAngle :: Point -> [Point] -> [Point]
sortPointsByAngle anchor = sortBy angleFromAnchor
    where angleFromAnchor p1 p2 = compare (cross u v) 0
            where u = vectorDifference anchor p1
                  v = vectorDifference anchor p2
