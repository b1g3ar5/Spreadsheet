{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

module Arithmetic ( Arithmetic(..), nFunctionMap) where

import Data.Map as M hiding (map, foldl, (!))
import Data.Maybe
import Data.Monoid
import Data.List hiding (and)
import Data.Char
import Data.Number.Erf
import Data.Time.Calendar

import Cat
import Ref
import Value
import Sheet


-- Now we will have 3 types of expression, arithmetic, logical and strings

-- | For number calculations
data Arithmetic e = AVal Double
	    | Add e e
		| Mul e e
		| Div e e
		| Sub e e
		| Pow e e 
		-- | This is a reference to a number cell in a sheet
		| NRef (Sheet e) Ref
		-- | This is a spreadsheet function eg. log()
		| NFunc String  [e] deriving (Show)

-- | Make all the expression types functors
instance Functor Arithmetic where
	fmap f (AVal x)  = AVal x
	fmap f (Add x y)  = Add (f x) (f y)
	fmap f (Mul x y)  = Mul (f x) (f y)
	fmap f (Div x y)  = Div (f x) (f y)
	fmap f (Sub x y)  = Sub (f x) (f y)
	fmap f (Pow x y)  = Pow (f x) (f y)
	fmap f (NFunc s ps) = NFunc s (map f ps)
	fmap f (NRef s r) = NRef (fmap f s) r

-- | The Arithmetic type is in the Eval class
instance (Monad m) => Eval Arithmetic m where
    evalAlg :: Arithmetic (m Value) -> m Value
    evalAlg (AVal x) = return $ N x
    evalAlg (Add x y) = do n <- x
                           m <- y
                           return $ n+m;
    evalAlg (Mul x y) = do n<-x
                           m<-y
                           return $ n*m
    evalAlg (Div x y) = do n<-x
                           m<-y
                           return $ n / m
    evalAlg (Sub x y) = do n<-x
                           m<-y
                           return $ n-m
    evalAlg (Pow x y) = do n<-x
                           m<-y
                           return $ n**m
    evalAlg (NRef s r) = s!r       
    evalAlg (NFunc s ps) = do
                    pss <- sequence ps
                    let ff = M.lookup (map toUpper s) nFunctionMap
                    return $ maybe (E "No such function") (\f -> f pss) ff



-- | Look up a function
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
                , ("ADDMONTHS", addMonths)
                , ("ADDYEARS", addYears)
              ]

-- | These are the built in spreadsheet functions

-- | The density function for the normal distribution
dnorm :: (Floating a) => [Value] -> Value
dnorm (x:[]) = 1/(2.0*pi)**0.5* (exp $ -(x*x/2.0))
dnorm _ = E "dnorm has only one argueent"

-- | The distribution function for the normal distribution
--   That is the integral of the density function.
pnorm :: [Value] -> Value
pnorm (x:[]) = 0.5 + 0.5 * erf(x / (sqrt 2.0)) 
pnorm _ = E "pnorm has only one argueent"

-- | log function
vln :: [Value] -> Value
vln (x:[]) = log x
vln _ = E "ln has only one argueent"

-- | exponential function
vexp :: [Value] -> Value
vexp (x:[]) = exp x
vexp _ = E "exp has only one argueent"

-- | pi
vpi :: [Value] -> Value
vpi [] = pi
vpi _ = E "pi has no arguments"

-- | e
ve :: [Value] -> Value
ve [] = exp 1.0
vw _ = E "e has no arguementa"

-- | Average of a list of numbers
--   Could do with generalising this to ranges
mean :: (Real a, Fractional a) => [a] -> a
mean xs = realToFrac (sum xs) / genericLength xs

-- | Variance of a list of numbers
var :: (Real a, Fractional a) => [a] -> a
var xs = m2 - m*m
    where
        m2 = mean $ map (\x->x*x) xs
        m = mean xs

-- | Standard deviation of a list of numbers
std :: (Real a, Fractional a, Floating a) => [a] -> a
std xs = sqrt (var xs)

addMonths :: [Value] -> Value
addMonths (N x:N y:[]) = N $ (fromIntegral  $ toModifiedJulianDay $ addGregorianMonthsClip (round x) $ ModifiedJulianDay $ round y)
addMonths _ = E "addmonths nees 2 numerical arguments"

addYears :: [Value] -> Value
addYears (N x:N y:[]) = N $ (fromIntegral  $ toModifiedJulianDay $ addGregorianYearsClip (round x) $ ModifiedJulianDay $ round y)
addYears _ = E "addmonths nees 2 numerical arguments"


