import Text.ParserCombinators.Parsec
import Control.Applicative hiding (many, optional, (<|>))
import Text.Parsec.Char(string)
import Numeric (readHex)
import Control.Monad (liftM2)

p_query :: CharParser () [(String, Maybe String)]
p_query = p_pair `sepBy` char '&'

p_pair :: CharParser () (String, Maybe String)
p_pair = liftM2 (,) (many1 p_char) (optionMaybe (char '=' >> many p_char))

p_char :: CharParser () Char
p_char = oneOf urlBaseChars
     <|> (char '+' >> return ' ')
     <|> p_hex

urlBaseChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "$-_.!*'(),"

p_hex :: CharParser () Char
p_hex = do
    char '%'
    a <- hexDigit
    b <- hexDigit
    let ((d, _):_) = readHex [a, b]
    return . toEnum $ d

a_hex :: CharParser () Char
a_hex = hexify <$> (char '%' *> hexDigit) <*> hexDigit
        where hexify a b = toEnum . fst . head . readHex $ [a, b]


