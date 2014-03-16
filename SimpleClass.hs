{-# LANGUAGE TypeSynonymInstances, OverlappingInstances, FlexibleInstances #-}

import Data.List

instance Foo String where 
    foo = id
class Foo a where
    foo :: a -> String

instance Foo a => Foo [a] where
    foo = concat . intersperse ", " . map foo

instance Foo Char where 
    foo = (:[])

