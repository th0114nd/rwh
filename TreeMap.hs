data Tree a = Node (Tree a) (Tree a)
            | Leaf a
              deriving (Show)

treeLengths :: Tree String -> Tree Int
treeLengths (Leaf s) = Leaf $ length s
treeLengths (Node x y) = Node (treeLengths x) (treeLengths y)


treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f (Leaf x) = Leaf $ f x
treeMap f (Node l r) = Node (treeMap f l) (treeMap f r)

{-
class Functor f where
    fmap :: (a -> b) -> f a -> f b
    -}
instance Functor Tree where
    fmap = treeMap
{-
instance Functor [] where
    fmap = map
    -}
