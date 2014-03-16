{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module JSONClass where
import Control.Arrow

type JSONError = String

newtype JAry a = JAry {
    fromJAry :: [a]
} deriving (Eq, Ord, Show)

newtype JObj a = JObj {
    fromJObj :: [(String, a)]
} deriving (Eq, Ord, Show)

data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject (JObj JValue) 
            | JArray (JAry JValue)
              deriving (Eq, Ord, Show)

jaryFromJValue :: (JSON a) => JValue -> Either JSONError (JAry a)
jaryFromJValue (JArray (JAry a)) = 
    whenRight JAry (mapEithers fromJValue a)
jaryFromJValue _ = Left "not a JSON array"

whenRight :: (b -> c) -> Either a b -> Either a c
whenRight f (Right b) = Right $ f b
whenRight _ (Left t) = Left t

mapEithers :: (a -> Either b c) -> [a] -> Either b [c]
mapEithers f (x:xs) = case mapEithers f xs of
                    Left err -> Left err
                    Right ys -> case f x of 
                            Left err -> Left err
                            Right t -> Right (t:ys)
mapEithers _ _ = Right []

jaryToJValue :: (JSON a) => JAry a -> JValue
jaryToJValue = JArray . JAry . map toJValue . fromJAry

instance (JSON a) => JSON (JObj a) where
    toJValue = JObject . JObj . map (second toJValue) . fromJObj

    fromJValue (JObject (JObj o)) = whenRight JObj (mapEithers unwrap o)
        where unwrap (k, v) = whenRight ((,) k) (fromJValue v)
    fromJValue _ = Left "Not a JSON object"

instance (JSON a) => JSON (JAry a) where
    toJValue = jaryToJValue
    fromJValue = jaryFromJValue

listToJValues :: (JSON a) => [a] -> [JValue]
listToJValues = map toJValue

jvaluesToJAry :: [JValue] -> JAry JValue
jvaluesToJAry = JAry

jaryOfJValuesToJValue :: JAry JValue -> JValue
jaryOfJValuesToJValue = JArray


class JSON a where 
    toJValue :: a -> JValue
    fromJValue :: JValue -> Either JSONError a

instance JSON JValue where
    toJValue = id
    fromJValue = Right

instance JSON Bool where
    toJValue = JBool
    fromJValue (JBool b) = Right b
    fromJValue _ = Left "not a JSON boolean"

instance JSON [Char] where
    toJValue = JString
    fromJValue (JString s) = Right s
    fromJValue _ = Left "not a JSON string"

doubleToJValue :: (Double -> a )-> JValue -> Either JSONError a
doubleToJValue f (JNumber d) = Right (f d)
doubleToJValue _ _ = Left "not a JSON number"

instance JSON Int where
    toJValue = JNumber . realToFrac
    fromJValue = doubleToJValue round
    
instance JSON Integer where  
    toJValue = JNumber . realToFrac
    fromJValue = doubleToJValue round

