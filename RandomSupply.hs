module RandomSupply where
import Supply
import System.Random hiding (next)

randomsIO :: Random a => IO [a]
randomsIO = 
    getStdRandom $ \g ->
        let (a, b) = split g
        in (randoms a, b)
