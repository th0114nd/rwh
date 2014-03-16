data Color = Red | Green | Blue
    deriving (Read, Show, Eq, Ord)

data CannotShow = CannotShow
            deriving (Show)

data CannotDeriveShow = CannotDeriveShow CannotShow
                         deriving (Show)

data OK = OK

instance Show OK where
    show _ = "OK"

data ThisWorks = ThisWorks OK deriving (Show)
