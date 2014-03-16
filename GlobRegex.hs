module GlobRegex
    (
       globToRegex
     , matchesGlob
     , GlobError
     ) where

import Data.Char
import Text.Regex.Posix ((=~))

type GlobError = String

globToRegex :: Bool -> String -> Either GlobError String
globToRegex p cs = globToRegex' p cs >>= return . ('^':) . (++ "$")

globToRegex' :: Bool -> String -> Either GlobError String
globToRegex' p "" =  return ""
globToRegex' p ('*':cs) = globToRegex' p cs >>= return . (".*" ++)
globToRegex' p ('?':cs) = globToRegex' p cs >>= return . ('.' :)
globToRegex' p ('[':'!':c:cs) = charClass p cs >>= 
                                return . ("[^" ++) . (c :) 
globToRegex' p ('[':c:cs) = charClass p cs >>= return . ('[' :) . (c :)
globToRegex' p ('[':_) = Left "untermninated character class"
globToRegex' p (c:cs) | p = globToRegex' p cs >>= 
                            return . (casePair (escape c) ++)
                      | otherwise = globToRegex' p cs >>=
                            return . (escape c ++)
  
casePair :: String -> String
casePair xs = "(" ++ (map toUpper xs) ++ "|" ++ (map toLower xs) ++ ")"

matchesGlob :: Bool -> String -> String -> Either GlobError Bool
matchesGlob cond name pat  = globToRegex cond pat >>= return . (name =~)

escape :: Char -> String
escape c | c `elem` regexChars = '\\' : [c]
         | otherwise = [c]
     where regexChars = "\\+()^$.{}]|"

charClass :: Bool -> String -> Either GlobError String
charClass p (']':cs) = globToRegex' p cs >>= return . (']':)
charClass p (c:cs) = do
                reg <- charClass p cs
                if p
                    then return (toUpper c : toLower c : reg)
                    else return (c : reg)
charClass p [] = Left "unterminated character class"

