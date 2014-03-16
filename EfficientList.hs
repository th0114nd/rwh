myDumbExample xs = if length xs > 0
                        then head xs
                        else 'Z'

lessDumb (x:_) = x
lessDumb _ = 'Z'


safeHead [] = Nothing
safeHead (x:xs) = Just x

safeTail [] = Nothing
safeTail (x:xs) = Just xs

safeLast [] = Nothing
safeLast (x:[]) = Just x
safeLast (x:xs) = safeLast xs

safeInit [] = Nothing
safeInit [x] = Just []
safeInit (x:xs) = safeInit xs >>= (\end -> return $ x:end)

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith f xs = let notf = not . f
                     newBeg = takeWhile notf xs
                     newEnd = dropWhile f (dropWhile notf xs)
                 in newBeg : splitWith f newEnd 
