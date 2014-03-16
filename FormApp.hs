import Text.ParserCombinators.Parsec
import Control.Applicative hiding (many, optional, (<|>))
import Text.Parsec.Char(string)
import Numeric (readHex)

a_hex :: CharParser () Char
a_hex = (hexify <$> (char '%' *> hexDigit)) <*> hexDigit
        
hexify :: Char -> Char -> Char
hexify a b = toEnum . fst . head . readHex $ [a, b]

urlBaseChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "$-_.!*'(),"

a_char = oneOf urlBaseChars
    <|> (' ' <$ char '+')
    <|> a_hex

a_pair :: CharParser () (String, Maybe String)
a_pair = liftA2 (,) (many1 a_char) (optionMaybe (char '=' *> many a_char))
