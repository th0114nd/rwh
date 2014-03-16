import Data.Maybe as DM
data Tree a = Node a (Tree a) (Tree a)
            | Empty deriving (Show)

simpleTree = Node "parent" (Node "Left child" Empty Empty)
       
                           (Node "Right Child" Empty Empty)

data MTree a = MNode a (Maybe (MTree a)) (Maybe (MTree a)) deriving (Show)

newSimple = MNode "parent" (Just (MNode "Left" Nothing Nothing))
                           (Just (MNode "Right" Nothing Nothing))    

convert :: Tree a -> Maybe (MTree a)
convert (Node x lf rt) = Just $ MNode x (convert lf) (convert rt)
convert Empty = Nothing 

height :: Tree a -> Integer
height Empty = 0
height (Node _ l r) = 1 + max (height l) (height r)

checkHeight = Node "x" Empty (Node "y" Empty Empty)
