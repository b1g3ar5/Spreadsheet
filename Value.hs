{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Value ( showWithFormat, Format(..), Value(..), Eval(..), eval, vadd, vand, vor, vxor, vgt, vlt, veq, ccat) where

import Data.String
import Data.Monoid
import Data.Number.Erf as E
import Data.Time.Calendar
import Data.Time.Format (formatTime, defaultTimeLocale)
import Text.Read (readMaybe)
--import System.Locale

import Cat
import Ref

-- | This is the type that a cell evaluates to. E is for ERROR and is not implemented yet
data Value where
    N :: Double -> Value
    B :: Bool -> Value
    S :: String -> Value
    R :: [Ref] -> Value
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
    pi  = N pi

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

-- | Add 2 numbers
vadd:: Value -> Value -> Value
vadd (N b) (N c) = N $ b + c
vadd _ _ = E "vadd needs number parameters"

-- | Boolean AND = &&
vand:: Value -> Value -> Value
vand (B b) (B c) = B $ b && c
vand _ _ = E "vand needs boolean parameters"

-- | Boolean OR = |
vor:: Value -> Value -> Value
vor (B b) (B c) = B $ b || c
vor _ _ = E "vand needs boolean parameters"

vors :: [Value] -> Value
vors [] = B False
vors (b:[]) = b
vors (b:c:bs) = vor (vor b c) $ vors bs

-- | Boolean XOR = 
vxor:: Value -> Value -> Value
vxor (B b) (B c) = B $ (b || c) && (not (b && c))
vxor _ _ = E "vand needs boolean parameters"


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
ccat _ _ = E "ccat needs string parameters"


-- | Formatting - we need to show Values with a format
-- We need number of decimal points and dates for the minumum
-- YMD = yyyy-mm-ss
-- DMY = dd-mmm-yy
-- FN n = n decimal places
-- Loads more formats we could add - commas, red for negative, brackets for negative etc.
data Format = FN Int | YMD | DMY

showWithFormat :: Format -> String -> String
showWithFormat (FN f) s = maybe s (\x -> show (fromIntegral (round (x*k)) / k)) $ readMaybe s
    where
        k = 10.0 ** (fromIntegral f)
showWithFormat YMD s = maybe s (\x -> show $ ModifiedJulianDay $ round x) $ readMaybe s
showWithFormat DMY s = maybe s (\x -> formatTime defaultTimeLocale "%d-%b-%y" $ ModifiedJulianDay $ round x) $ readMaybe s
        
instance Read Format where
    readsPrec _ s = [(maybe (FN 2) id $ ff s, "")]
        where
            ff :: String -> Maybe Format
            ff "YMD" = Just YMD
            ff "DMY" = Just DMY
            ff s = fmap (FN) (readMaybe s)



