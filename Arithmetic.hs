{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

module Arithmetic ( Arithmetic(..), nFunctionMap) where

import Data.Map as M hiding (map, foldl, (!))
import Data.Maybe
import Data.List hiding (and)
import Data.Char
import Data.Number.Erf

import Cat
import Refs
import Value
import Sheet


-- Now we will have 3 types of expression, arithmetic, logical and strings

-- |For number calculations
data Arithmetic e = AVal Double
	    | Add e e
		| Mul e e
		| Div e e
		| Sub e e
		| Pow e e 
		-- |This is a reference to a number cell in a sheet
		| NRef (Sheet e) Ref
		-- |This is a spreadsheet function eg. log()
		| NFunc String  [e] deriving (Show)

-- |Make all the expression types functors
instance Functor Arithmetic where
	fmap f (AVal x)  = AVal x
	fmap f (Add x y)  = Add (f x) (f y)
	fmap f (Mul x y)  = Mul (f x) (f y)
	fmap f (Div x y)  = Div (f x) (f y)
	fmap f (Sub x y)  = Sub (f x) (f y)
	fmap f (Pow x y)  = Pow (f x) (f y)
	fmap f (NFunc s ps) = NFunc s (map f ps)
	fmap f (NRef s r) = NRef (fmap f s) r

-- |The Arithmetic type is in the Eval class
instance (Monad m) => Eval Arithmetic m where
	evalAlg :: Arithmetic (m Value) -> m Value
	evalAlg (AVal x) = return $ N x
	evalAlg (Add x y) = x >>= \n ->
			    y >>= \m ->
			    return $ n+m
	evalAlg (Mul x y) = x >>= \n ->
			    y >>= \m ->
			    return $ n*m
	evalAlg (Div x y) = x >>= \n ->
			    y >>= \m ->
			    return $ n / m
	evalAlg (Sub x y) = x >>= \n ->
			    y >>= \m ->
			    return $ n-m
	evalAlg (Pow x y) = x >>= \n ->
			    y >>= \m ->
			    return $ n**m
	evalAlg (NRef s r) = s!r       
	evalAlg (NFunc s ps) = do
					pss <- sequence ps
                    -- **** NEED TO DEAL WITH Nothing HERE ****
					let ff = fromJust $ M.lookup (map toUpper s) nFunctionMap
					return $ ff pss;

-- |Look up a function
nFunctionMap :: Map String ([Value]->Value)
nFunctionMap = fromList [   
                  ("SUM", sum)
                , ("EXP", vexp)
                , ("LOG", vln)
                , ("PI", vpi)
                , ("E", ve)
                , ("MEAN", mean)
                , ("STD", std)
                , ("VAR", var)
                , ("DNORM", dnorm)
                , ("PNORM", pnorm)
              ]

-- |These are the built in spreadsheet functions

-- |The density function for the normal distribution
dnorm :: (Floating a) => [a] -> a
dnorm (x:xs) = 1/(2.0*pi)**0.5* (exp $ -(x*x/2.0))

-- |The distribution function for the normal distribution
-- That is the integral of the density function.
pnorm :: (Erf a) => [a] -> a
pnorm (x:xs) = 0.5 + 0.5 * erf(x / (sqrt 2.0)) 

-- |log function
vln :: (Floating a) => [a] -> a
vln (x:xs) = log x

-- | exponential function
vexp :: (Floating a) => [a] -> a
vexp (x:xs) = exp x

-- |pi
vpi :: (Floating a) =>  [a] -> a
vpi = const pi

-- |e
ve :: (Floating a) =>  [a] -> a
ve = const $ exp 1.0

-- |Average of a list of numbers
-- Could do with generalising this to ranges
mean :: (Real a, Fractional a) => [a] -> a
mean xs = realToFrac (sum xs) / genericLength xs

-- |Variance of a list of numbers
var :: (Real a, Fractional a) => [a] -> a
var xs = m2 - m*m
    where
        m2 = mean $ map (\x->x*x) xs
        m = mean xs

-- |Standard deviation of a list of numbers
std :: (Real a, Fractional a, Floating a) => [a] -> a
std xs = sqrt (var xs)



