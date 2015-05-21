{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Value ( Value(..), Eval(..), eval, vand, vor, vxor, vgt, vlt, veq, ccat) where

import Cat
import Data.String
import Data.Number.Erf as E

-- This is the type that a cell evaluates to. E is for ERROR and is not implemented yet
data Value where
	N :: Double -> Value
	B :: Bool -> Value
	S :: String -> Value
	E :: String -> Value deriving (Eq, Ord)

-- Make Value instances of a few general classes

-- Make Value an instance of some number classes if it's a Double (ie. N x)
instance Num Value where
	fromInteger i = N (fromInteger i)
	(N x) + (N y) = N (x+y)
	(N x) * (N y) = N (x*y)
	negate (N x) = N (-x)
	abs (N x) = N (abs x)
	signum (N x) = N (signum x)

instance Fractional Value where
	recip (N x) = N (1/x)
	fromRational r = N $ fromRational r

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

instance Real Value where
	toRational (N v) = toRational v

instance Erf Value where
    erf (N v) = N $ erf v


instance IsString Value where
	fromString s = S s

-- This will be used for the display of the calculated cell values
-- so we leave out all the type stuff
instance Show Value where
	show (N x) = show x
	show (B b) = show b
	show (S s) = s
	show (E e) = "Error: " ++ show e

-- Make an Eval class that these 3 types can be instances of
-- This is a f-algebra type, so our eval function just calls
-- cata evalAlg
class (Monad m, Functor f) => Eval f m where
	evalAlg :: f (m Value) -> m Value

-- An eval function that applies the algebra to the Fix type
-- This is cata from the Data.Fix library
eval :: (Monad m, Eval f m) => Fix f -> m Value
eval = cata evalAlg

-- Some bool functions that work on Values (when they are bools)
vand:: Value -> Value -> Value
vand (B b) (B c) = B $ b && c

vor:: Value -> Value -> Value
vor (B b) (B c) = B $ b || c

vxor:: Value -> Value -> Value
vxor (B b) (B c) = B $ (b || c) && (not (b && c))

vgt:: Value -> Value -> Value
vgt (N b) (N c) = B $ (b > c) 
vgt (S b) (S c) = B $ (b > c) 

vlt:: Value -> Value -> Value
vlt (N b) (N c) = B $ (b < c)
vlt (S b) (S c) = B $ (b < c) 

veq:: Value -> Value -> Value
veq (N b) (N c) = B $ (b == c) 
veq (S b) (S c) = B $ (b == c) 
veq (B b) (B c) = B $ (b == c) 

ccat:: Value -> Value -> Value
ccat (S b) (S c) = S $ b ++ c

