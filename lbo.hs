lastButOne :: [a] -> a
lastButOne [] = error "List too short"
lastButOne (x:y:[]) = x
lastButOne (x:xs) = lastButOne xs
