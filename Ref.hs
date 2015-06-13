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

module Ref ( Col(..), Row(..), Ref(..), toCoords, fromCoords, refAdd, rLeft, rRight, rUp, rDown, rZero, readRef ) where



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

-- This file has all the stuff to do with references/addresses in spreadsheets
-- Absolute, relative, adding them, using them as indices etc.

-- This is for array types
data Col = CRel {x::Int} | CAbs {x::Int} deriving (Eq)
data Row = RRel {y::Int} | RAbs {y::Int} deriving (Eq)
data Ref = Ref {cRef::Col, rRef::Row} deriving (Eq)

-- Col 'A' is 1, so show adds 64 to get 'A'
instance Show Col where
    show (CRel x) = [chr (64+x)]
    show (CAbs x) = [chr (64+x)]

instance Show Row where
    show (RRel x) = show x
    show (RAbs x) = show x

-- Assumes counting starts at 1, so the 'A' column is 1
cMod :: Col -> Col -> Col
cMod (CAbs n) (CRel x) = CRel $ 1 + ((x-1) `mod` n)
cMod (CAbs n) (CAbs x) = CAbs $ 1 + ((x-1) `mod` n)
cMod (CRel n) (CRel x) = CRel $ 1 + ((x-1) `mod` n)
cMod (CRel n) (CAbs x) = CAbs $ 1 + ((x-1) `mod` n)

rMod :: Row -> Row -> Row
rMod (RAbs n) (RRel x) = RRel $ 1 + ((x-1) `mod` n)
rMod (RAbs n) (RAbs x) = RAbs $ 1 + ((x-1) `mod` n)
rMod (RRel n) (RRel x) = RRel $ 1 + ((x-1) `mod` n)
rMod (RRel n) (RAbs x) = RAbs $ 1 + ((x-1) `mod` n)

refMod :: Ref -> Ref -> Ref
refMod (Ref cmax rmax) (Ref c r) = Ref (cMod cmax c) (rMod rmax r)

instance Show Ref where
    show (Ref c r) = show c ++ show r

instance Monoid Ref where
    mempty = rZero
    mappend = refAdd (Ref (CAbs 10) (RAbs 10))

rLeft :: Ref
rLeft = Ref (CRel $ -1) (RRel 0)

rRight :: Ref
rRight = Ref (CRel 1) (RRel 0)

rUp :: Ref
rUp = Ref (CRel 0) (RRel $ -1)

rDown :: Ref
rDown = Ref (CRel 0) (RRel 1)

rZero :: Ref
rZero = Ref (CRel 0) (RRel 0)


toCoords :: Ref -> (Int, Int)
toCoords ref = (x $ cRef ref, y $ rRef ref)

fromCoords :: (Int, Int) -> Ref
fromCoords (x,y) = Ref (CRel $ x) (RRel $ y)

-- toCoords . fromCoords = id

data Arr e = Arr { arrFrom ::Ref, arrTo::Ref}

-- | Addition of references
-- If either of the y corrds are Abs then it's just them
-- The final Ref uses the types of the y
-- This will be used for copying cells and things like that.
cadd:: Col -> Col -> Col -> Col
cadd cmax c1 c2@(CAbs c) = cMod cmax c2
cadd cmax c1@(CRel c) c2 = cMod cmax  $ CRel $ (x c1) + (x c2)
cadd cmax c1@(CAbs c) c2 = cMod cmax  $ CAbs $ (x c1) + (x c2)

radd :: Row -> Row -> Row -> Row
radd rmax c1 c2@(RAbs _) = rMod rmax c2
radd rmax c1 c2 = rMod rmax $ RRel $ (y c1) + (y c2)

-- This is bad - we need to disallow adding to absolute references in the type system
refAdd :: Ref -> Ref -> Ref -> Ref
refAdd (Ref cmax rmax) r1 (r2@(Ref (CAbs _) (RAbs _))) = r2
refAdd (Ref cmax rmax) r1 (r2@(Ref (CAbs _) (RRel _))) = Ref (cRef r2) $ radd rmax (rRef r1) (rRef r2)
refAdd (Ref cmax rmax) r1 (r2@(Ref (CRel _) (RAbs _))) = Ref (cadd cmax (cRef r1) $ cRef r2) (rRef r2) 
refAdd (Ref cmax rmax) r1 (r2@(Ref (CRel _) (RRel _))) = Ref (cadd cmax (cRef r1) $ cRef r2) (radd rmax (rRef r1) $ rRef r2)  

-- toCoords/fromCoords defines the ordering
instance Ord Ref where
	compare x y = compare (toCoords x) $ toCoords y

instance Ix Ref where
	range (a, b) = fmap fromCoords $ range (toCoords a, toCoords b)
	inRange (a, b) c = inRange (toCoords a, toCoords b) $ toCoords c
	index (m,n) p = index (toCoords m, toCoords n) $ toCoords p

readRef :: String -> Ref
readRef ss = fromCoords (col, row)
    where
        col = (ord $ head ss) - 64
        row = read $ tail ss


