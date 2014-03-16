import Data.List (isPrefixOf)

dlts :: String -> [String]

dlts = foldr step [] . lines
    where step line coll = if ("#define" `isPrefixOf`) line
                                then (secondWord line) : coll
                                else coll
          secondWord = head . tail . words
