main = putStrLn "Greetings! name?"
   >>  getLine
   >>= (\inp -> putStrLn $ "welcome to haskell, " ++ inp ++ "!")
