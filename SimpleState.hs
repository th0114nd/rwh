type SimpleState s a  = s -> (a, s)

type StringState a = SimpleState String a

retrnST a = \s -> (a, s)

returnALt a s = (a, s)

bindSt :: (SimpleState s a) -> (a -> SimpleState s b) -> SimpleState s b
bindSt m f st = let (a1, s1) = m st
                in f a1 s1

bindAlt step makeStep oldState = 
    let (result, newState) = step OldState
    in (makeStep result) newState

getSt :: SimpleState s s
getSt = \s -> (s, s)

putSt :: s -> SimpleState s ()
putSt s = const ((), s)


