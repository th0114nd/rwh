import Data.List
import qualified Data.Map as M
import System.IO
import Text.Printf(printf)
import System.Environment(getArgs)
import System.Exit
import Control.Monad(when)
import PasswdAL (split)

data PasswdEntry = PasswdEntry {
    userName :: String,
    password :: String,
    uid :: Integer,
    gid :: Integer,
    gecos :: String,
    homeDir :: String,
    shell :: String
} deriving (Eq, Ord)

instance Show PasswdEntry where
    show pe = printf "%s:%s:%d:%d:%s:%s:%s:%s"
                     (userName pe) (password pe) (uid pe) (gid pe)
                     (gecos pe) (homeDir pe) (shell pe)

instance Read PasswdEntry where 
    readsPrec _ value =
        case split ':' value of
            [f1, f2, f3, f4, f5, f6, f7] -> 
                [(PasswdEntry f1 f2 (read f3) (read f4) f5 f6 f7, [])]
            x -> error $ "Invalid number of fields in input: " ++ show x

type UIDMap = M.Map Integer PasswdEntry
type UserMap = M.Map String PasswdEntry

inputToMaps :: String -> (UIDMap, UserMap)
inputToMaps inp = (uidmap, usermap)
    where uidmap = M.fromList . map (\pe -> (uid pe, pe)) $ entries
          usermap = M.fromList . map (\pe -> (userName pe, pe)) $ entries
          entries = map read (lines inp)

main = do
    args <- getArgs
    when (length args /= 1) $ do
        putStrLn "Syntax: passwdmap filename"
        exitFailure
    content <- readFile (head args)
    let maps = inputToMaps content
    mainMenu maps

mainMenu maps@(uidmap, usermap) = do
    putStr optionText
    hFlush stdout
    sel <- getLine
    case sel of
        "1" -> lookupUserName >> mainMenu maps
        "2" -> lookupUID  >> mainMenu maps 
        "3" -> displayFile >> mainMenu maps 
        "4" -> return ()
        _ -> putStrLn "invalid selection" >> mainMenu maps

    where 
    lookupUserName = do
        putStr "Username: "
        username <- getLine
        case M.lookup username usermap of
            Nothing -> putStrLn "Not found."
            Just x -> print x
    lookupUID = do
        putStr "UID: "
        uidstring <- getLine
        case M.lookup (read uidstring) uidmap of
            Nothing -> putStrLn "Not found."
            Just x -> print x
    displayFile = putStr . unlines . map (show .snd) . M.toList $ uidmap
    optionText = 
        "\npasswdmap options:\n\
        \\n\
        \1   Look up a user name\n\
        \2   Look up a UID\n\
        \3   Display entire file\n\
        \4   Quit\n\n\
        \Your selection: "

