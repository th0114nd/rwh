module ControlledVisit where
import Control.Monad (filterM, liftM, forM)
import System.Directory (Permissions(..), getModificationTime, 
                        getPermissions, getDirectoryContents)
import System.Time (ClockTime(..))
import System.FilePath (takeExtension, (</>))
import Control.Exception (bracket, handle, SomeException)
import System.IO (IOMode(..), hClose, hFileSize, openFile)
import System.Posix (getFileStatus, fileOwner)
import BetterPredicate

data Info = Info {
    infoPath :: FilePath
  , infoPerms :: Maybe Permissions
  , infoSize :: Maybe Integer
  , infoModTime :: Maybe ClockTime
  , infoOwner :: UserID
} deriving (Eq, Ord, Show)



traverse :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverse order path = do
    names <- getUsefulContents path
    contents <- mapM getInfo (path : map (path </>) names)
    liftM concat $ forM (order contents) $ \info -> do
        if isDirectory info && infoPath info /= path
            then traverse order (infoPath info)
            else return [info]

traverseAndFilter :: ([Info] -> [Info]) -> FilePath -> (Info -> Bool) -> IO [Info]
traverseAndFilter order path p = do
         paths <- traverse order path
         return $ filter p paths

transfer :: InfoP a -> Info -> Maybe a
transfer f i = do
       perms <- infoPerms i
       time <- infoModTime i
       return $ f (infoPath i)
                  perms
                  (infoSize i)
                  time
                  (infoOwner i)

getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
    names <- getDirectoryContents path
    return (filter (`notElem` [".", ".."]) names)

isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms

maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handle ((\_ -> return Nothing)::Handler a) 
                     (Just `liftM` act)

getInfo :: FilePath -> IO Info
getInfo path = do
    perms <- maybeIO (getPermissions path)
    size <- maybeIO (bracket (openFile path ReadMode) hClose hFileSize)
    modified <- maybeIO (getModificationTime path)
    return (Info path perms size modified)


traverseVerbose order path = do
    names <- getDirectoryContents path
    let usefulNames = filter (`notElem` [".", ".."]) names
    contents <- mapM getEntryName ("" : usefulNames)
    recursiveContents <- mapM recurse (order contents)
    return (concat recursiveContents)
  where getEntryName name = getInfo (path </> name)
        isDirectory info = case infoPerms info of
                            Nothing -> False
                            Just perms -> searchable perms
        recurse info = do
            if isDirectory info && infoPath info /= path
                then traverseVerbose order (infoPath info)
                else return [info]
