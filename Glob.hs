{-# LANGUAGE ScopedTypeVariables #-}
module Glob (namesMatching) where
import System.Directory (doesDirectoryExist, doesFileExist,
                         getCurrentDirectory, getDirectoryContents)
import System.FilePath (dropTrailingPathSeparator, splitFileName, (</>))
import Control.Exception (handle, SomeException)
import Control.Monad (forM, filterM)
import GlobRegex (matchesGlob, GlobError)
import System.FilePath (pathSeparator)

namesMatching :: String -> IO (Either GlobError [String])
namesMatching pat
    | not (isPattern pat) = do
        exists :: Bool <- doesNameExist pat
        return $ Right (if exists then [pat] else [])
    | otherwise = do
        case splitFileName pat of 
            ("", baseName) -> do
                curDir <- getCurrentDirectory
                listMatches curDir baseName
            (dirName, baseName) -> do
                dirs :: Either GlobError [String] <- if isPattern dirName
                        then namesMatching (dropTrailingPathSeparator dirName)
                        else return $ Right [dirName]
                let listDir = if isPattern baseName
                              then listMatches
                              else listPlain
                pn :: [String] <- do 
                    pathNames <- do 
                       dirs `eitherBind` (\ds -> 
                           forM ds $ \dir -> do
                               baseNames <- listDir dir baseName
                               return (map (dir </>) baseNames))
                    return pathNames :: Either GlobError [String] 
                return ((pn >>= return . concat) :: Either GlobError [String])

eitherBind :: Either GlobError a -> (a -> Either GlobError b) -> Either GlobError b
eitherBind = (>>=)

isPattern :: String -> Bool
isPattern = any (`elem` "[*?")

listMatches :: FilePath -> String -> IO (Either GlobError [String])
listMatches dirName pat = do
    dirName' <- if null dirName
                then getCurrentDirectory
                else return dirName
    handle (const (return $ Right []) :: 
                SomeException -> IO (Either GlobError [String])) $
       do
        names <- getDirectoryContents dirName'
        let names' = if isHidden pat
                        then filter isHidden names
                        else filter (not . isHidden) names
        return (filterM doesMatch names')
            where doesMatch :: FilePath -> Either GlobError Bool
                  doesMatch fp = matchesGlob False fp pat

listPlain :: FilePath -> String -> IO (Either GlobError [String])
listPlain dirName baseName =do
    exists <- if null baseName
                 then doesDirectoryExist dirName
                 else doesNameExist (dirName </> baseName)
    return $ Right (if exists then [baseName] else [])
       

isHidden :: String -> Bool
isHidden ('.':_) = True
isHidden _ = False

doesNameExist :: FilePath -> IO Bool
doesNameExist name = do
            fileExists <- doesFileExist name
            if fileExists
                then return True
                else doesDirectoryExist name
