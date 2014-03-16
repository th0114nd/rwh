module State where
newtype State s a = State {
    runState :: s -> (a, s)
}

returnState :: a -> State s a
returnState x = State $ (\s -> (x, s))

bindState :: State s a -> (a -> State s b) -> State s b
bindState step makeStep = State $ \oldState ->
                            let (val, newState) = runState step oldState 
                            in runState (makeStep val) newState

instance Monad (State s) where
    (>>=) = bindState
    return = returnState

get :: State s s
get = State $ \x -> (x, x)

put :: s -> State s ()
put y = State $ \_ -> ((), y)

modify :: (s -> s) -> State s ()
modify f = get >>= put . f

