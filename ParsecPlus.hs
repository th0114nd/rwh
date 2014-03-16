{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

import Control.Monad
import Text.ParserCombinators.Parsec
instance MonadPlus (GenParser tox st) where
    mzero = fail "mzero"
    mplus = (<|>)
