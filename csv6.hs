import Text.ParserCombinators.Parsec

csvFile = line `endBy` eol
line = cell `sepBy` (char ',')
cell = many (noneOf ",\n\r")

eol =  try (string "\n\r")
   <|> try (string "\r\n")
   <|> string "\n"
   <|> string "\r"
   <?> "end of line"
    
parseCSV :: String -> Either ParseError [[String]]
parseCSV = parse csvFile "(unknown)" 
