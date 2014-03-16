import System.IO
import Data.Char(toUpper)

main :: IO ()
main = do
    inh <- openFile "input.txt" ReadMode
    outh <- openFile "output.txt" WriteMode
    inpStr <- hGetContents inh
    let result =  reverse $ inpStr
    hPutStr outh result
    hClose inh
    hClose outh
