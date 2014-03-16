myConcat :: [[a]] -> [a]
myConcat = foldr (++) []

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile _ [] = []
myTakeWhile p (x:xs) = if p x
                         then x : (myTakeWhile p xs)
                         else []

foldTakeWhile :: (a -> Bool) -> [a] -> [a]
foldTakeWhile p = foldr step []
    where step x xs = if p x 
                         then x : xs 
                         else [] 
myGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
myGroupBy comp xs = foldr step [] xs
    where step x ((y:ys):xss) 
            | comp x y = (x:y:ys):xss
            | otherwise   = [x]:(y:ys):xss
          step x xss = [x]:xss

myCycle :: [a] -> [a]
myCycle xs = foldr (++) (myCycle xs) [xs] 

myAny :: (a -> Bool) -> [a] -> Bool
myAny p xs = foldr (\x b -> p x || b) False xs

myWords :: String -> [String]
myWords = foldr step []
    where step c xss | isWhiteSpace c = []:xss
          step c (xs:xss) = (c:xs):xss
          step c [] = [[c]]

isWhiteSpace :: Char -> Bool
isWhiteSpace c = or [ c == ' ' , c == '\n', c == '\t'] 

unlines :: [String] -> String
unlines = foldr (\l p -> l ++ '\n':p) [] 
