{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFoldable #-}

module Refs ( Refs(..)) where

import Data.Map as M hiding (map, foldl, (!))
import Data.Maybe
import Data.Monoid
import Data.Foldable
import Data.List hiding (and)
import Data.Array hiding ((!))
import Data.Char
import Cat
import Ref
import Value
import Sheet

-- | The expression for calculating circular references
-- The e is a dummy variable - needed for all the spreadsheet recalculation stuff
-- ie. The Eval class
data Refs e = Refs [Ref] deriving (Show, Foldable)

-- Not really a functor - since the e is a dummy type we can't apply a general f
-- to the [Ref] that we have.
instance Functor Refs where
	fmap f (Refs x)  = Refs x

instance Monad m => Eval Refs m where
    evalAlg :: Refs (m Value) -> m Value
    evalAlg (Refs xs) = return $ R xs



