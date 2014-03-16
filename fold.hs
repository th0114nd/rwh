fakeFilt p = foldr step []
    where step a b = if p a
                        then a : b
                        else b

myMap f = foldr (\x ys -> f x : ys) [] 

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f init [] = init
myFoldl f init (x:xs) = myFoldl f (f init x) xs

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f init [] = init
myFoldr f init (x:xs) = f x (myFoldr f init xs)


myFoldl' _ zero [] = zero
myFoldl' step zero (x:xs) = 
    let new = step zero x
    in new `seq` myFoldl' step new xs

{-
    hiddenInside x y = sumoFunc (x `seq` y)

    hiddenByLet x y z = let a = x `seq` someFunc y
                        in anotherFunc a z

    onTheOutside x y = x `seq` someFunc y
---}

chained f x y z = x `seq` y `seq` f z

strictPair (a, b) = a `seq` b `seq` (a, b)

strictList (x:xs)  = x `seq` x : strictList xs
strictList [] = []
