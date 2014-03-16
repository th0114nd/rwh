mySecond :: [a] -> a
mySecond xs = if null (tail xs)
                then error "Too short!"
                else head (tail xs)

safeSecond :: [a] -> Maybe a
safeSecond xs = if (null xs) || (null (tail xs))
                    then Nothing
                    else Just $ head $ tail xs

tidySecond (_:x:_) = Just x
tidySecond _ = Nothing
