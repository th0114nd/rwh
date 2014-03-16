module Logger (
                Logger
              , Log
              , runLogger
              , record
              ) where
import Control.Monad

type Log = [String]
newtype Logger a = Logger { execLogger :: (a, Log) }

runLogger :: Logger a -> (a, Log)
runLogger = execLogger

record :: String -> Logger ()
record s = Logger ((), [s])

instance Monad Logger where
    return a = Logger (a, [])
    m >>= k = let (a, w) = execLogger m
                  n = k a
                  (b, x) = execLogger n
              in Logger (b, w ++ x)

globToRegex :: String -> Logger String
globToRegex cs = do
    ds <- globToRegex' cs
    return ('^':ds)

globToRegex' :: String -> Logger String
globToRegex' "" = return "$"
globToRegex' ('?':cs) = do
    record "any"
    ds <- globToRegex' cs
    return ('.':ds)
globToRegex' ('*':cs) = do
    record "kleene star"
    ds <- globToRegex' cs
    return (".*" ++ ds)
globToRegex' ('[':'!':c:cs) = do
    record "character class, negative" 
    ds <- charClass cs
    return ("[^" ++ c : ds)
globToRegex' ('[':c:cs) = do
    record "character class, postive"
    ds <- charClass cs
    return ('[':c:ds)
globToRegex' ('[':_) =
    fail "unterminated character class"
globToRegex' (c:cs) = liftM2 (++) (escape c) (globToRegex' cs)

escape :: Char -> Logger String
escape c 
    | c `elem` regexChars = record "escape" >> return ['\\', c]
    | otherwise = return [c]
  where regexChars = "\\+()^$.{}]|"

charClass :: String -> Logger String
charClass ('[':_) = fail "unterminated character class"
charClass (']':cs) = (']':) `liftM` globToRegex' cs
charClass (c:cs) = (c:) `liftM` charClass cs
