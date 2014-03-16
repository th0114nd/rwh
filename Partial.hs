{ use -XNoMonomorphismRestriction }
import Data.List
{-
 - isInAny needle haystack = any inSequence haystack
    where inSequence s = needle `isInfixOf` s
    -}

isInAny needle = (any . isInfixOf) needle
