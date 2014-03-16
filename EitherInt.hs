{-# LANGUAGE FlexibleInstances #-}
instance Functor (Either a) where
    fmap _ (Left n) = Left n
    fmap f (Right r) = Right (f r)
    

