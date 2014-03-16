module Prettify where
import Debug.Trace

data Doc = Empty
         | Char Char
         | Text String
         | Nest Int Doc
         | Line
         | Concat Doc Doc
         | Union Doc Doc
            deriving (Show, Eq)


(<>) :: Doc -> Doc -> Doc
Empty <> y = y
x <> Empty = x
x <> y = x `Concat` y

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

char :: Char -> Doc
char c = Char c

compact :: Doc -> String
compact x = transform [x]
    where transform []  = ""
          transform (d:ds) = 
               case (trace (show d) d) of
                 Empty -> transform ds
                 Char c -> c : transform ds
                 Text s -> s ++ transform ds
                 Nest i x -> replicate i ' ' ++ transform (x:ds)
                 Line -> '\n' : transform ds
                 a `Concat` b -> transform (a:b:ds)
                 _ `Union` b -> transform (b:ds)

double :: Double -> Doc
double num = text (show num)

empty :: Doc
empty = Empty

fill :: Int -> Doc -> Doc
fill width x = iter 0 [x]
    where iter col (d:ds) = 
            case d of 
                Empty -> iter col ds
                Char c -> Char c <> iter (col + 1) ds
                Text s -> Text s <> iter (col + length s) ds
                Nest i x -> Nest i (iter (col + i) (x:ds))
                Line -> genSpaces (width - col) <> Line <> iter 0 ds
                a `Concat` b -> iter col (a:b:ds)
                a `Union` b -> (iter col (a:ds)) `Union` (iter col (b:ds))
          iter _ _ = Empty

          genSpaces :: Int -> Doc
          genSpaces x | x <= 0 = Empty
                      |  otherwise = text (replicate x ' ')

fits :: Int -> String -> Bool
w `fits` _ | w < 0 = False
w `fits` "" = True
w `fits` ('\n':_) = True
w `fits` (c:cs) = (w - 1) `fits` cs

flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten line = Char ' '
flatten (Nest i x) = Nest i (flatten x)
flatten (x `Union` _) = flatten x
flatten other = other

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

fsep :: [Doc] -> Doc
fsep = fold (</>)

group :: Doc -> Doc
group x = flatten x `Union` x

hcat :: [Doc] -> Doc
hcat = fold (<>)

line :: Doc
line = Line

nest :: Int -> Doc -> Doc
nest = Nest

pretty :: Int -> Doc -> String
pretty width x = best 0 [x]
    where best col (d:ds) = 
            case d of 
                Empty -> best col ds
                Char c -> c : best (col + 1) ds
                Text s -> s ++ best (col + length s) ds
                Nest i x -> (replicate i ) ' ' ++ (best (col + i) $ x:ds)
                Line -> '\n' : best 0 ds
                a `Concat` b -> best col (a:b:ds)
                a `Union` b -> nicest col (best col (a:ds))
                                          (best col (b:ds))
          best _ _ = ""

          nicest :: Int -> String -> String -> String
          nicest col a b | (width - least) `fits` a = a
                         | otherwise = b
                         where least = min width col

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p [] = []
punctuate p [d] = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds

softline :: Doc
softline = group line

text :: String -> Doc
text "" = Empty
text str = Text str
