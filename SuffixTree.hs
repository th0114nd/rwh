import Data.List

suffixes :: [a] -> [[a]]
suffixes xs@(_:xs') = xs : suffixes xs'
suffixes _ = []

noAsPattern (x:xs) = (x:xs) : noAsPattern xs
noAsPattern _ = []

suffixes2 :: [a] -> [[a]]
suffixes2 xs = init (tails xs)

suffixes3 = compose init tails

suffixes5 = init . tails

compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)
