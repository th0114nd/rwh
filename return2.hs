import Data.Char(toUpper)

isYes :: String -> Bool
isYes inpStr = (toUpper . head $ inpStr) == 'Y'

isGreen :: IO Bool
isGreen = 
    do putStrLn "Is gren ur fav?"
       inpStr <- getLine
       return (isYes inpStr)
