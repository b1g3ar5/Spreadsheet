{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Value ( VF, Format, Value(..), Eval(..), eval, vand, vor, vxor, vgt, vlt, veq, ccat) where

import Data.String
import Data.Number.Erf as E
import Data.Time.Calendar

import Cat

-- | This is the type that a cell evaluates to. E is for ERROR and is not implemented yet
data Value where
	N :: Double -> Value
	B :: Bool -> Value
	S :: String -> Value
	E :: String -> Value deriving (Eq, Ord)

-- | Make Value an instance of Num
instance Num Value where
	fromInteger i = N (fromInteger i)
	(N x) + (N y) = N (x+y)
	(N x) * (N y) = N (x*y)
	negate (N x) = N (-x)
	abs (N x) = N (abs x)
	signum (N x) = N (signum x)

-- | Make Value an instance of Fractional
instance Fractional Value where
	recip (N x) = N (1/x)
	fromRational r = N $ fromRational r

-- | Make Value an instance of Floating
instance Floating Value where
    	pi  = N pi
	exp (N x) = N $ exp x
	log (N x) = N $ log x
	sin (N x) = N $ sin x
	cos (N x) = N $ sin x
	asin (N x) = N $ asin x
	acos (N x) = N $ acos x
	atan (N x) = N $ atan x
	sinh (N x) = N $ sinh x
	cosh (N x) = N $ cosh x
	asinh (N x) = N $ asinh x
	acosh (N x) = N $ acosh x
	atanh (N x) = N $ atanh x

-- | Make Value an instance of Real
instance Real Value where
	toRational (N v) = toRational v

-- | Make Value an instance of Erf
instance Erf Value where
    erf (N v) = N $ erf v

-- | Make Value an instance of IsString
instance IsString Value where
	fromString s = S s

-- | A Show instance
instance Show Value where
	show (N x) = show x
	show (B b) = show b
	show (S s) = s
	show (E e) = "Error: " ++ show e

-- | Make an Eval class that these 3 types can be instances of
--   This is a f-algebra type, so our eval function just calls
--   cata evalAlg
class (Monad m, Functor f) => Eval f m where
	evalAlg :: f (m Value) -> m Value

-- | An eval function that applies the algebra to the Fix type
--   This is cata from the Data.Fix library
eval :: (Monad m, Eval f m) => Fix f -> m Value
eval = cata evalAlg

-- | Some spreadsheet functions that work on Values - but have special characters in the cell input

-- | Boolean AND = &&
vand:: Value -> Value -> Value
vand (B b) (B c) = B $ b && c
vand _ _ = E "vand need boolena parameters"

-- | Boolean OR = |
vor:: Value -> Value -> Value
vor (B b) (B c) = B $ b || c

-- | Boolean XOR = 
vxor:: Value -> Value -> Value
vxor (B b) (B c) = B $ (b || c) && (not (b && c))

-- | Boolean greater than = >
vgt:: Value -> Value -> Value
vgt (N b) (N c) = B $ (b > c) 
vgt (S b) (S c) = B $ (b > c) 

-- | Boolean less than = <
vlt:: Value -> Value -> Value
vlt (N b) (N c) = B $ (b < c)
vlt (S b) (S c) = B $ (b < c) 

-- | Boolean equal = ==
veq:: Value -> Value -> Value
veq (N b) (N c) = B $ (b == c) 
veq (S b) (S c) = B $ (b == c) 
veq (B b) (B c) = B $ (b == c) 

-- | String concatenation = &
ccat:: Value -> Value -> Value
ccat (S b) (S c) = S $ b ++ c

-- Formatting - we need to show Values with a format
-- We need number of decimal points and dates for the minumum
-- so We'll have a -1 for dates and any other number for decimal points then
-- we can have:
type Format = Int

-- We'll have a typr for formatted output

type VF = (Value, Format)

-- Then we cam habe a special version of show

instance Show VF where
	show ((N x), t) = if t==(-1) then (show $ ModifiedJulianDay $ round x) else show $ fromIntegral (round $ x*k) /k
	    where k = 10.0**(fromIntegral t)
	show ((B b), t) = show b
	show ((S s), t) = s
	show ((E e), t) = "Error: " ++ show e




