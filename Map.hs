import Data.Char (toUpper)

square :: (Num a) => [a] -> [a]
square [] = []
square (x:xs) = x*x : square xs

upperCase :: String -> String
upperCase = map toUpper
