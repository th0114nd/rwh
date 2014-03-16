module PNM where 
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Char (isSpace)
import Control.Monad (guard)

 
parseP5 :: L.ByteString -> Maybe (Greymap, L.ByteString) 
parseP5 s = 
    case matchHeader (L8.pack "P5") s of
        Nothing -> Nothing
        Just s1 -> 
            case getNat s1 of
                Nothing -> Nothing
                Just (width, s2) -> 
                    case getNat (L8.dropWhile isSpace s2) of
                        Nothing -> Nothing
                        Just (height, s3) ->
                            case getNat (L8.dropWhile isSpace s3) of
                                Nothing -> Nothing
                                Just (maxGrey, s4)
                                    | maxGrey > 255 -> Nothing
                                    | otherwise ->
                                        case getBytes 1 s4 of 
                                            Nothing -> Nothing
                                            Just (_,s5) -> 
                                                case getBytes (width * height) s5 of
                                                    Nothing -> Nothing
                                                    Just (bitmap, s6) -> 
                                                        Just (Greymap width height maxGrey bitmap, s6)

(>>?) :: Maybe a -> (a -> Maybe b) -> Maybe b
(>>?) Nothing _ = Nothing
(>>?) (Just x) f = f x

parseP5_take2 :: L.ByteString -> Maybe (Greymap, L.ByteString)
parseP5_take2 s = 
    matchHeader (L8.pack "P5") s >>?
    \s -> skipSpace ((), s)   >>?
    (getNat . snd) >>?
    skipSpace >>?
    \(width, s) -> getNat s >>?
    skipSpace >>?
    \(height, s) -> getNat s >>?
    skipSpace >>?
    \(maxGrey, s) -> getBytes 1 s >>?
    (getBytes (width * height) . snd) >>?
    \(bitmap, s) -> Just (Greymap width height maxGrey bitmap, s)

skipSpace :: (a, L.ByteString) -> Maybe (a, L.ByteString)
skipSpace (a, s) = Just (a, L8.dropWhile isSpace s)


    
parseP5_take3 :: L.ByteString -> Maybe (Greymap, L.ByteString) 
parseP5_take3 s =  do
    s1 <- matchHeader (L8.pack "P5") s
    (width, s2) <- getNat s1
    (height, s3) <- getNat (L8.dropWhile isSpace s2)
    (maxGrey, s4) <- getNat (L8.dropWhile isSpace s3)
    guard $ maxGrey > 255
    (_, s5) <- getBytes 1 s4
    (bitmap, s6) <- getBytes (width * height) s5
    return (Greymap width height maxGrey bitmap, s6)

matchHeader :: L.ByteString -> L.ByteString -> Maybe L.ByteString
matchHeader prefix str 
    | prefix `L8.isPrefixOf` str
        = Just (L8.dropWhile isSpace (L.drop (L.length prefix) str))
    | otherwise 
        = Nothing


getNat :: L.ByteString -> Maybe (Int, L.ByteString)
getNat s = do 
    (num, rest) <- L8.readInt s
    guard $ num > 0
    return (fromIntegral num, rest)

getBytes :: Int -> L.ByteString
        -> Maybe (L.ByteString, L.ByteString)
getBytes n str = let count = fromIntegral n
                     both@(prefix, _) = L.splitAt count str
                 in guard (L.length prefix < count) >> 
                    return both
 
