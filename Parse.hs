module Parse (
    (==>),
    (==>&),
    parseNat,
    skipSpaces,
    identity,
    assert,
    w2c,
    parseWhileWith,
    parse,
    parseByte,
    Parse(..),
    parseTimes
    )   where 
import Data.Int (Int64)
import Data.Word (Word8)
import Data.Char
import Control.Monad (liftM)
import Control.Monad.Instances
import Control.Applicative ((<$>))
import GHC.Base (chr)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8

data Greymap = Greymap {
     greyWidth :: Int
   , greyHeight :: Int
   , greyMax :: Int
   , greyData :: L.ByteString
} deriving (Eq)

instance Show Greymap where
    show (Greymap w h m _) = "Greymap " ++ show w ++ "x" ++ show h ++ 
                             " " ++ show m
data ParseState = ParseState {
    string :: L.ByteString
  , offset :: Int64
} deriving (Show)

newtype Parse a = Parse {
    runParse :: ParseState -> Either String (a, ParseState) 
}

instance Functor Parse where
    fmap f parser = parser ==> \result -> identity (f result)

simpleParse :: ParseState -> (a, ParseState)
simpleParse = undefined

betterParse :: ParseState -> Either String (a, ParseState)
betterParse = undefined

identity :: a -> Parse a
identity a = Parse (\s -> Right (a, s))

parse :: Parse a -> L.ByteString -> Either String a
parse parser initState = runParse parser (ParseState initState 0) >>=
                         return . fst

modifyOffset :: ParseState -> Int64 -> ParseState
modifyOffset initState n = initState {offset = n}

parseByte :: Parse Word8
parseByte = 
    getState ==> \initState ->
    case L.uncons (string initState) of
        Nothing -> 
            bail "no more input"
        Just (byte, remainder) ->
            putState newState ==> \_ ->
            identity byte
          where newState = initState { string = remainder,
                                       offset = newOffset }
                newOffset = offset initState + 1

(==>) :: Parse a -> (a -> Parse b) -> Parse b
firstParser ==> secondParser = Parse chainedParser
  where chainedParser initState = 
            case runParse firstParser initState of
                Left errMessage -> Left errMessage
                Right (firstResult, newState) -> 
                    runParse (secondParser firstResult) newState
infix 1 ==>


bail :: String -> Parse a
bail err = Parse $ \s -> Left $
           "byte offset " ++ show (offset s) ++ ": " ++ err

getState :: Parse ParseState
getState = Parse (\s -> Right (s, s))

putState :: ParseState -> Parse ()
putState s = Parse (\_ -> Right ((), s)) 

w2c :: Word8 -> Char
w2c = chr . fromIntegral

parseChar :: Parse Char
parseChar = w2c <$> parseByte


peekByte :: Parse (Maybe Word8)
peekByte = (fmap fst . L.uncons . string) <$> getState


peekChar :: Parse (Maybe Char)
peekChar = fmap w2c <$> peekByte

parseWhile :: (Word8 -> Bool) -> Parse [Word8]
parseWhile p = (fmap p <$> peekByte) ==> \mp ->
               if mp == Just True 
                    then parseByte ==> \b ->
                         (b:) <$> parseWhile p
                    else identity []


parseWhileVerbose p = 
    peekByte ==> \mc ->
    case mc of
        Nothing -> identity []
        Just c | p c ->
                    parseByte ==> \b ->
                    parseWhileVerbose p ==> \bs ->
                    identity (b:bs)
               | otherwise ->
                    identity []

parseRawPGM = 
    parseWhileWith w2c notWhite ==> \header -> skipSpaces ==>&
    assert (header == "P5") "invalid raw header" ==>&
    parseNat ==> \width -> skipSpaces ==>&
    parseNat ==> \height -> skipSpaces ==>&
    parseNat ==> \maxGrey ->
    parseByte ==>&
    parseBytes (width * height) ==> \bitmap ->
    identity (Greymap width height maxGrey bitmap)

notWhite = (`notElem` " \r\n\t")

parsePlainPGM = 
    parseWhileWith w2c notWhite ==> \header -> skipSpaces ==>&
    assert (header == "P2") "invalid plain header" ==>&
    parseNat ==> \width -> skipSpaces ==>&
    parseNat ==> \height -> skipSpaces ==>&
    parseNat ==> \maxGrey ->
    parseByte ==>&
    fmap (L.pack . map fromIntegral) (parseNats (width * height)) ==> \bitmap ->
    identity (Greymap width height maxGrey bitmap)


parseWhileWith :: (Word8 -> a) -> (a -> Bool) -> Parse [a]
parseWhileWith f p = fmap f <$> parseWhile (p . f)

parseNat :: Parse Int
parseNat = parseWhileWith w2c isDigit ==> \digits ->
          if null digits
              then bail "no more input"
              else let n = read digits
                   in if n < 0
                        then bail "integer overflow"
                        else identity n

(==>&) :: Parse a -> Parse b -> Parse b
p ==>& f = p ==> \_ -> f

skipSpaces :: Parse ()
skipSpaces = parseWhileWith w2c isSpace ==>& identity ()

assert :: Bool -> String -> Parse ()
assert True _ = identity ()
assert False err = bail err




parseBytes :: Int -> Parse L.ByteString
parseBytes n = 
    getState ==> \st ->
    let n' = fromIntegral n
        (h, t) = L.splitAt n' (string st)
        st' = st {offset = offset st + L.length h, string = t }
    in putState st' ==>&
       assert (L.length h == n') "end of input" ==>&
       identity h

parseNats :: Int -> Parse [Int]
parseNats  0 = identity []
parseNats k = parseNat ==> \n ->
             (n:) <$> parseNats (k - 1)

parseTimes :: Int -> Parse a -> Parse [a]
parseTimes 0 _ = identity []
parseTimes n p = p ==> \x -> (x:) <$> parseTimes (n - 1) p
