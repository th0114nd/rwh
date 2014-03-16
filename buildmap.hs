import qualified Data.Map as M

al = [(1, "one"), (2, "two"), (3, "three"), (4, "four")]

mapFromAL = M.fromList al

mapFold = 
    foldl (\map (k, v) -> M.insert k v map) M.empty al

mapManual = 
    M.insert 2 "two" .
    M.insert 4 "four" .
    M.insert 1 "one" .
    M.insert 3 "there" $ M.empty

