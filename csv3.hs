import Text.ParserCombinators.Parsec
import Text.Parsec.Char

eol :: GenParser Char st Char
eol = char '\n' >> (char '\r' <|> return '\n')

