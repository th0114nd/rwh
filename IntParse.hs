import Data.Char (ord)

asInt :: String -> Int
asInt ('-':ss) = negate (asInt ss)
asInt ss = if (all (\v -> 0 <= v && v <= 9) valued)
                then foldl (\a b -> 10 * a + b) 0 valued
                else error "Couldn't parse input string :("
             where valued = map (\char -> ord char - ord '0') ss

type ErrorMessage = String
asInt_either :: String -> Either Int ErrorMessage
asInt_either ('-':ss) = case asInt_either ss of
    Left x -> Left (negate x)
    Right e -> Right e
asInt_either ss = if (all (\v -> 0 <= v && v <= 9) valued)
                then Left $ foldl (\a b -> 10 * a + b) 0 valued
                else Right "Couldn't parse input string :("
             where valued = map (\char -> ord char - ord '0') ss

