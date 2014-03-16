import ControlledVisit
data Iterate seed = Done { unwrap :: seed }
                  | Skip { unwrap :: seed }
                  | Continue {unwrap :: seed }
                    deriving (Show)

type Iterator seed = seed -> Info -> Iterate seed

foldTree :: ([FilePath] -> [FilePath]) -> Iterator a -> a -> FilePath -> IO a

foldTree order iter initSeed path = fold initSeed path >>= return . unwrap
  where
    fold seed subpath = getUsefulContents subpath >>= walk seed . order

    walk seed (name:names) = do
        let path' = path </> name
        info <- getInfo path'
        case iter seed info of
            done@(Done _) -> return done
            Skip seed' -> walk seed' names
            Continue seed'
                | isDirectory info -> do
                    next <- fold seed' path'
                    case next of 
                        done@(Done _) -> return done
                        seed'' -> walk (unwrap seed'') names
                | otherwise -> walk seed' names

atMostThreePictures :: Iterator [FilePath]

atMostThreePictures paths info
    | length paths == 3
      = Done paths
    | isDirectory info && takeFileName pah == ".svn"
      = Skip paths
    | extension `elems` [".jpg", ".png"]
      = Continue (path : paths)
    | otherwise
      = Continue paths
   where extension = map . toLower (takeExtension path)
         path = infoPath info


countDirectories count info = 
    Continue (if isDirectory info
                then count + 1
                else count)
