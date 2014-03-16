import Prelude hiding ((>>=), return)

class Functor m => AltMonad m where
    join :: m (m a) -> m a
    return :: a -> m a

(>>=) :: Altmonad m => m a -> (a -> m b) -> m b
xs >>= f = join (fmap f xs)

join :: Monad m => m (m a) -> m a
join x = x >>= id







