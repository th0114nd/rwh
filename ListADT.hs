data Lst a = Cons a (Lst a)
           | Nil
             deriving (Show)

hdea (Cons x xs) = x
hdea Nil = error "No head :("

fromList :: [a] -> Lst a
fromList (x:xs) = Cons x (fromList xs)
fromList [] = Nil

toList :: Lst a -> [a]
toList Nil = []
toList (Cons x xs) = (:) x (toList xs)
