import qualified Data.ByteString.Lazy as L

hasElfMagic :: L.ByteString -> Bool
hasElfMagic content = l.take 4 content == elfMagic
    where elfMagic = L.pack[0x7f, 0x45, 0x4c, 0x46]

isElfFile :: FilePath -> IO Bool
isElfFile path = L.readFile path >>= return . hasElfMagic
