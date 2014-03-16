{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HandleIO
    (
      HandleIO
    , Handle
    , IOMode(..)
    , runHandleIO
    , openFile
    , hClose
    , hPutStrLn
    ) where

import System.IO (Handle, IOMode(..))
import qualified System.IO
import Control.Monad.Trans (MonadIO(..))
import System.Directory

newtype HandleIO a = HandleIO { runHandleIO :: IO a}
    deriving (Monad)

instance MonadIO HandleIO where
    liftIO = HandleIO

hPutStrLn :: Handle -> String -> HandleIO ()
hPutStrLn path str = HandleIO (System.IO.hPutStrLn path str)

hClose :: Handle -> HandleIO ()
hClose path = HandleIO (System.IO.hClose path)

openFile :: FilePath -> IOMode -> HandleIO Handle
openFile path mode = HandleIO (System.IO.openFile path mode)


safeHello :: FilePath -> HandleIO ()
safeHello path = do
    h <- openFile path WriteMode
    hPutStrLn h "Hello world"
    hClose h

tidyHello :: FilePath -> HandleIO ()
tidyHello path = do
    safeHello path
    liftIO (removeFile path)
