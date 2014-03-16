import Test.QuickCheck
import MonadHandle
import WriterIO
import System.IO (IOMode(..))
import SafeHello
import Data.Set as DS

prop_safe :: WriterIO a -> Bool 
prop_safe action = let (val, cats) = runWriterIO action 
                   in wellOrdered cats

dangerousHello :: (MonadHandle h m) => m () 
dangerousHello = do
    h <- openFile "input.txt" WriteMode
    hClose h
    hPutStrLn h "hello world"

wellOrdered :: [Event] -> Bool
wellOrdered es = iter DS.empty es
    where iter :: DS.Set String -> [Event] -> Bool
          -- First arg is a list of open files
          iter open [] | open == DS.empty = True
                       | otherwise = False
          iter open ((Open fp _):es) = iter (DS.insert fp open) es
          iter open ((Put fp _):es) = if fp `DS.member` open 
                                        then iter open es
                                        else False
          iter open ((Close fp):es) = if fp `DS.member` open
                                        then iter (DS.delete fp open) es
                                        else False
          iter open ((GetContents fp):es) = if fp `DS.member` open
                                        then iter open es
                                        else False
