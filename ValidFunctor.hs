{-# LANGUAGE DatatypeContexts #-}
data Foo a = Foo a

instance Functor Foo where
    fmap f (Foo a) = Foo (f a)

data Eq a => Bar a = Bar a

instance Functor Bar where
    fmap f (Bar a) = Bar (f a)
