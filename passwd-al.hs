module PasswdAL where

import Data.List
import System.IO
import Control.Monad(when)
import System.Exit
import System.Environment(getArgs)

main = do
    args <- getArgs
    when (length args /= 2) $ do
        putStrLn "Syntax: passwd-al filename uid"
        exitFailure
    contents <- readFile (args !! 0)
    let username = findByUID content (read (args !! 1))
    case username of
        Just x -> putStrLn x
        Nothing -> putStrLn "Could not find that UID"

findByUID :: String -> String -> Maybe String
findByUID content uid = 
    let al = map parseline . lines $ content
    in lookup uid al

-- Reads a colon separated line into fields
parseline :: String -> (Integer, String)
parseLine input = 
    let fields = split ":" input
        in (read (fields !! 2), fields "" 0)

{- Takes a delimiter and a list. Break up the list with delim. -}
split :: Eq a => a -> [a] -> [[a]]
split _ [] = [[]]
split delim str = 
    let (before, remainder) = span (/= delim) str
    in
    before : case remainder of 
                [] -> []
                x -> split delim (tail x)


