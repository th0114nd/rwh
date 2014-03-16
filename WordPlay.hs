module WordPlay where
import Debug.Trace
firstWords = unlines . (map head) . (map words) . lines 

transpose = unlines . reduce . lines 

takeFirsts :: [[Char]] -> [Char]
takeFirsts [] = []
takeFirsts ([]:xs) = ' ':takeFirsts xs
takeFirsts ((c:cs):xs) = c:(takeFirsts xs)

takeLasts :: [[Char]] -> [[Char]]
takeLasts [] = []
takeLasts ([]:xs) = takeLasts xs
takeLasts ((c:cs):xs) = cs:(takeLasts xs)

reduce :: [[Char]] -> [[Char]]
reduce [] = []
reduce ss = (takeFirsts ss) : (reduce (takeLasts ss))
