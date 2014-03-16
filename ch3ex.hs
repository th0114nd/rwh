import Data.List
import Tree
len :: [a] -> Integer
len [] = 0
len (x:xs) = 1 + len xs

mean :: (Fractional a) => [a] -> a
mean [] = error "No mean for empty list"
mean xs = (sum xs) / (fromIntegral (len xs))

palindrome :: [a] -> [a]
palindrome xs = xs ++ reverse xs

isPal :: (Eq a) => [a] -> Bool
isPal xs = xs == reverse xs

sortLens :: [[a]] -> [[a]]
sortLens = sortBy lengComp

lengComp :: [a] -> [a] -> Ordering
lengComp [] (x:xs) = LT
lengComp (y:ys) [] = GT
lengComp [] [] = EQ
lengComp (y:ys) (x:xs) = lengComp ys xs

is :: a -> [[a]] -> [a]
is _ [] = []
is n [x] = x
is n (x:xs) = x ++ n:(is n xs)
