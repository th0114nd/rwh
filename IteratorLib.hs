module IteratorLib where 
import System.Directory (Permissions(..))
import System.Time (ClockTime(..))
data Info = Info {
    infoPath :: FilePath
  , infoPerms :: Maybe Permissions
  , infoSize :: Maybe Integer
  , infoModTime :: Maybe ClockTime
} deriving (Eq, Ord, Show)

data Iterate seed = Done { unwrap :: seed }
                  | Skip { unwrap :: seed }
                  | Continue {unwrap :: seed }
                    deriving (Show)

type Iterator seed = seed -> Info -> Iterate seed

createIter :: (a -> Info -> Bool) -> 
              (a -> Info -> Bool) -> 
              (a -> a) ->
              Iterator a
createIter stop skip transfer seed 
    | stop seed = Done seed
    | skip seed = Skip seed
    | otherwise = Continue $ transfer seed
        
noContextStop :: (a -> Bool) -> Iterator a
noContextStop stop seed 
    | stop seed = Done seed
    | otherwise = Continue seed


