{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

module Refs ( Refs(..)) where

import Data.Map as M hiding (map, foldl, (!))
import Data.Maybe
import Data.Monoid
import Data.List hiding (and)
import Data.Array hiding ((!))
import Data.Char
import Cat
import Ref
import Value
import Sheet

-- | The expression for string calculations
data Refs e = RVal [Ref]
		| RRef (Sheet e) Ref deriving (Show)

instance Functor Refs where
	fmap f (RVal x)  = RVal x
	fmap f (RRef s r) = RRef (fmap f s) r


instance Monad m => Eval Refs m where
    evalAlg :: Refs (m Value) -> m Value
    evalAlg (RVal xs) = return $ R xs
    evalAlg (RRef s r) = s!r       

