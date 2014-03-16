safeHead (x:_) = Just x
safeHead _ = Nothing

unsafeHead :: [a] -> Maybe a
unsafeHead xs = return $ head xs 
