{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Expr ( Sheet(..), eval, Eval(..), Value(..), Arithmetic(..), Logic(..), Str(..), (:<:)(..), (:+:), finject, inject, nFunctionMap, sFunctionMap, bFunctionMap ) where

import Text.PrettyPrint hiding ((<>))
import Data.Map as M hiding (map, foldl, (!))
import Data.Maybe
import Data.List hiding (and)
import Data.Monoid
import Data.Function
import Data.Array
import Data.String
import Data.Char

import Cat
import Refs
import Value
import Arithmetic
import Logic
import Str
import Sheet


-- | We have 3 types of expression which are described in the Arithmetic, Logic and Str types
-- This file has the apparatus to help in the creation of the type after parsing.
-- It is copied from Huttons paper "Towards modular compilers for effects"
-- which is based on W. Swierstra's "Data Types `a la Carte", I think
-- I'm not sure it's entirely necessary in this case - but at least it makes the
-- code a bit neater.

-- |This operator applies either the left or right function
-- This is the coproduct of 2 functors
-- It's like a disjoint sum - you get back one or the other
data (f :+: g) e = Inl (f e) | Inr (g e)

-- |If f and g are functors then so is the 'sum' of them
instance (Functor f, Functor g) => Functor ( f :+: g) where
	fmap f (Inl x) = Inl (fmap f x)
	fmap g (Inr y) = Inr (fmap g y)

-- |A class for injecting a value from a sub type into a sup type
class (Functor sub, Functor sup) => sub :<: sup where
	inj :: sub a -> sup a

-- |A simple instance - we can inject a value from a type into itself
instance (Functor f) => f :<: f where
	inj = id

-- |Inject from f into the 'sum' of f with g
-- the value goes into the left which is the f that it's in
instance (Functor f, Functor g) => f :<: (f :+: g) where
	inj = Inl

-- |Inject from f into the 'sum' of h with g that we know how to inject into from f
-- the value goes into the right which is the g after we've called inj to put the value in g
instance (Functor h, f :<: g) => f :<: (h :+: g) where
	inj = Inr . inj

-- |Make a num instance for the Fix of an f
instance (Num (f (Fix f))) => Num (Fix f) where
	fromInteger i = Fix $ (fromInteger i)
	x + y = Fix (unFix x+ unFix y)
	x * y = Fix (unFix x* unFix y)
	negate x = Fix (- unFix x)
	abs x = Fix (abs $ unFix x)
	signum x = Fix (signum $ unFix x)

-- |Make a fractional instance for the Fix of an f
instance (Fractional (f (Fix f))) => Fractional (Fix f) where
	recip x = Fix (1/(unFix x))
	fromRational r = Fix $ fromRational r

-- |Two useful utility functions
inject = Fix . inj
finject = const . inject












