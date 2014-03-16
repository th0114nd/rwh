name2reply name = 
    "Pleased to meet you, " ++ name ++ ".\n" ++
    "Your name countains " ++ charcount ++ " characters."
    where charcount = show . length $ name

main :: IO ()
main = putStrLn "Greetings. Name?" >> getLine >>= putStrLn . name2reply
