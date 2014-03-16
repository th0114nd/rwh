data MyMaybe a = No
               | Yes a

chain :: m a -> (a -> m b) -> m b
inject :: a -> m a
