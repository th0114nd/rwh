{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverlappingInstances #-}
import SimpleJSON
import JSONClass

instance (JSON a) => JSON [a] where
    toJValue = JArray . map toJValue 
    fromJValue = undefined 

instance (JSON a) => JSON [(String, a)] where
    toJValue = JObject . map (\(s, x) -> (s, toJValue x))
    fromJValue = undefined 
