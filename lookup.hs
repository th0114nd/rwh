myLookup :: Eq a => a -> [(a, b)] -> Maybe b
myLookup key [] = Nothing
myLookup key ((k, v):ps) | key == k = Just v
                         | otherwise = myLookup key ps
