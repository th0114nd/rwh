str2action :: String -> IO ()
str2action input = putStrLn ("Data: " ++ input)

list2actions :: [String] -> [IO ()]
list2actions = map str2action

numbers :: [Int]
numbers = [1..10]

strings :: [String]
strings = map show numbers

actions :: [IO ()]
actions = list2actions strings

printitall :: IO ()
printitall = runall actions

runall :: [IO ()] -> IO ()
runall = foldr (>>) (return ())

main = do str2action "Startoftheprogram"
          head actions
          printitall
          str2action "Done!" 
