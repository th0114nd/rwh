main = do
       putStrLn "Greetings!"
       inpStr <- getLine
       putStrLn $ "Welcome to Haskell, " ++ inpStr ++ "|"
