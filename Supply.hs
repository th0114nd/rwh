{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Supply
    (
      Supply
    , next
    , runSupply
    ) where
import State

runSupply :: Supply s a -> [s] -> (a, [s])
runSupply (S m) xs = runState m xs

next :: Supply s (Maybe s)
next = S $ do st <- get
              case st of 
                [] -> return Nothing
                (x:xs) -> do put xs
                             return . return $ x

newtype Supply s a = S (State [s] a)
    deriving (Monad)

unwrapS :: Supply s a -> State [s] a
unwrapS (S s) = s

{-
instance Monad Supply s where
    return a = S . return
    s >>= f = S (unwrapS s >>= unwrapS . m)
    -}

showTwo :: (Show s) => Supply s String
showTwo = do
    a <- next
    b <- next
    return ("a: " ++ show a ++ ", b: " ++ show b)   
