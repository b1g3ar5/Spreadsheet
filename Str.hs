{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

module Str ( Str(..), sFunctionMap) where

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
data Str e = SVal String 
		| Concat e e
		| SRef (Sheet e) Ref
		| SFunc String  [e] deriving (Show)

instance Functor Str where
	fmap f (SVal x)  = SVal x
	fmap f (Concat x y)  = Concat (f x) (f y)
	fmap f (SFunc s ps) = SFunc s (map f ps)


instance Monad m => Eval Str m where
    evalAlg :: Str (m Value) -> m Value
    evalAlg (SVal x) = return $ S x
    evalAlg (Concat x y) = x >>= \n ->
                y >>= \m ->
                return $ ccat n m
    evalAlg (SRef s r) = s!r       
    evalAlg (SFunc s ps) = do
                    pss <- sequence ps
                    let ff = M.lookup (map toUpper s) sFunctionMap
                    return $ maybe (E "No such function") (\f -> f pss) ff;

-- | Look up a built in string function
sFunctionMap :: Map String ([Value]->Value)
sFunctionMap = fromList [   ("SUBSTR", subStr)
              ]

subStr :: [Value] -> Value
subStr [] = E "need 3 parameters for subStr, 2 numbers and a string"
subStr (N start:N len: S s:[]) = S $ take (floor len) $ drop (floor start) s
subStr _ = E "need 3 parameters for subStr, 2 numbers and a string"


