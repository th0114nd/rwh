import Control.Monad
x `zeroMod` n = guard ((x `mod` n) == 0) >> return x
