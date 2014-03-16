module SafeHello where

import MonadHandle
import System.IO (IOMode(..))

import WriterIO
--safeHello :: MonadHandle h m => FilePath -> m ()
safeHello :: FilePath -> WriterIO ()
safeHello path = do
    h <- openFile path (WriteMode :: IOMode)
    hPutStrLn h "hello world"
    hClose h


