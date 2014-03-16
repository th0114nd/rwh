import Text.ParserCombinators.Parsec
import Text.Parsec.Char

csvFile = endBy line eol
line = sepBy cell (char ',')
cell = quotedCell <|> many (noneOf ",\n\r")

quotedCell = do
            char '"'
            content <- many quotedChar
            char '"' <?> "quote at end of cell"
            return content

quotedChar = noneOf "\"" <|> try (string "\"\"" >> return '"')

eol = try (string "\n\r")
  <|> try (string "\r\n")
  <|> string "\n"
  <|> string "\r"
  <?> "end of line"

parseCSV :: String -> Either ParseError [[String]]
parseCSV = parse csvFile "(unknown)"

main = do
    c <- getContents
    case parse csvFile "(stdin)" c of
         Left e -> putStrLn "Error parsing input:" >> print e
         Right r -> mapM_ print r
