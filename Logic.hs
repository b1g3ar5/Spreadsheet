{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

module Logic ( Logic(..), bFunctionMap) where

import Data.Map as M hiding (map, foldl, (!))
import Data.Maybe
import Data.List hiding (and)
import Data.Array
import Data.Char
import Cat
import Refs
import Value
import Sheet

-- For Boolean calculations
data Logic e = LVal Bool
		| And e e
		| Or e e
		| LGT e e
		| LLT e e
		| LEQ e e
		| Xor e e
		| BRef (Sheet e) Ref
		| BFunc String  [e] deriving (Show)

-- |It's a functor
instance Functor Logic where
	fmap f (LVal x)  = LVal x
	fmap f (And x y)  = And (f x) (f y)
	fmap f (Or x y)  = Or (f x) (f y)
	fmap f (Xor x y)  = Xor (f x) (f y)
	fmap f (LGT x y)  = LGT (f x) (f y)
	fmap f (LLT x y)  = LLT (f x) (f y)
	fmap f (LEQ x y)  = LEQ (f x) (f y)
	fmap f (BFunc s ps) = BFunc s (map f ps)

-- |And we can evaluate it
instance Monad m => Eval Logic m where
	evalAlg :: Logic (m Value) -> m Value
	evalAlg (LVal x) = return $ B x
	evalAlg (And x y) = x >>= \n ->
			    y >>= \m ->
			    return $ vand n m
	evalAlg (Or x y) = x >>= \n ->
			    y >>= \m ->
			    return $ vor n m
	evalAlg (Xor x y) = x >>= \n ->
			    y >>= \m ->
			    return $ vxor n m
	evalAlg (LGT x y) = x >>= \n ->
			    y >>= \m ->
			    return $ vgt n m
	evalAlg (LLT x y) = x >>= \n ->
			    y >>= \m ->
			    return $ vlt n m
	evalAlg (LEQ x y) = x >>= \n ->
			    y >>= \m ->
			    return $ veq n m
	evalAlg (BFunc s ps) = do
					pss <- sequence ps
					let ff = fromJust $ M.lookup (map toUpper s) bFunctionMap
					return $ ff pss;

-- |Look up a function
bFunctionMap :: Map String ([Value]->Value)
bFunctionMap = fromList [   ("NOT", vnot)
              ]

-- | A boolean spreadsheet function
vnot::[Value]->Value
vnot [] = B True
vnot ((B x):[]) = B $ not x
vnot _ = E "Only one parameter, please"


