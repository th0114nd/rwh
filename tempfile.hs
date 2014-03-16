import System.IO
import System.Directory(getTemporaryDirectory, removeFile)
import Control.Exception(finally)


-- entry point. Work with temp file
main = withTempFile "mytemp.txt" myAction

myAction :: FilePath -> Handle -> IO ()
myAction tempname temph = 
    do -- Start off friendly
       putStrLn "Welcome to tempfile.hs"
       putStrLn $ "I have a temp file at " ++ tempname

       --What is the initial position
       pos <- hTell temph
       putStrLn $ "My initial position is " ++ show pos

       -- Now write to the file
       let tempdata = show [1..13]
       putStrLn $ "Writing one line containing " ++
                  show (length tempdata) ++ " bytes: " ++
                  tempdata
       hPutStrLn temph tempdata

       -- What the actual position is.
       pos <- hTell temph
       putStrLn $ "After writing, my new position is " ++ show pos

       -- Go back to the beginning
       hSeek temph AbsoluteSeek 0

       -- hGetContents is lazy read
       c <- hGetContents temph
       putStrLn c

       putStrLn $ "Which is literally: "
       print c

withTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withTempFile pattern func = do
    tempdir <- catch (getTemporaryDirectory) (\_ -> return ".")
    (tempfile, temph) <- openTempFile tempdir pattern
    finally (func tempfile temph)
            (do hClose temph
                removeFile tempfile)
