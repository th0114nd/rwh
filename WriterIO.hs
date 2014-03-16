{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module WriterIO where
import Control.Monad.Writer
import System.IO(IOMode(..))
--import SafeHello
import MonadHandle

data Event = Open FilePath IOMode
           | Put String String
           | Close String
           | GetContents String
             deriving (Show)

newtype WriterIO a = W { runW :: Writer [Event] a }
    deriving (Monad, MonadWriter [Event])

runWriterIO :: WriterIO a -> (a, [Event])
runWriterIO = runWriter . runW

instance MonadHandle String WriterIO where
    openFile path mode = tell [Open path mode] >> return path
    hPutStr h str = tell [Put h str]
    hClose h = tell [Close h]
    hGetContents h = tell [GetContents h] >> return "fake contents"
