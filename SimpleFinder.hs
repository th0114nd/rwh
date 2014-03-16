import RecursiveContents (getRecursiveContents)

simpleFind :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
simpleFind p path = do
    names <- getRecursiveContents apth
    return $ filter p names
