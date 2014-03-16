import Data.Char (toUpper)

main = readFile "input.txt" >>= writeFile "output.txt" . map toUpper
