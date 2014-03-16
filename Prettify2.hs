module Prettify2 where
import Data.Monoid(Monoid, mappend, mempty)

data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
         deriving (Show, Eq)

empty :: Doc
empty = Empty

line :: Doc
line = Line

instance Monoid Doc where
    mappend = (<>)
    mempty = empty

(<>) :: Doc -> Doc -> Doc
Empty <> y = y
x <> Empty = x
x <> y = x `Concat` y

double :: Double -> Doc
double num = text (show num)

char :: Char -> Doc
char c = Char c

text :: String -> Doc
text "" = Empty
text str = Text str

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

hcat :: [Doc] -> Doc
hcat = fold (<>)

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p [] = []
punctuate p [d] = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds
