
guarded :: Bool -> [a] -> [a]
guarded True xs = xs
guarded False _ = []

multiplyTo :: Int -> [(Int, Int)]
multiplyTo n = do
    x <- [1..n]
    y <- [1..n]
    guarded (x * y == n) $ return (x, y) 
