{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

module Sheet (Sheet(..), (Sheet.!), shift) where

import Control.Monad.ST
import Data.Array.ST

import Data.Array 
import Data.Maybe
import Data.List
import Control.Monad
import Control.Monad.Identity
import Control.Comonad
import Text.Printf
import Data.Either.Utils (fromRight)
import Cat
import Refs
import Value
import Text.PrettyPrint hiding ((<>))


-- This file is experimenting with different versions of spreadsheets
-- We need the type to be Sheet (Sheet c -> c) so that cfix and loeb work

-- Now, the a in Sheet a needs to be (Sheet c -> c)
-- But the parser parses to CellFn which is Sheet Fix Cell -> Fix Cell
-- So the c is Fix Cell and we have a sheet of CellFn's - ie. Sheet CellFn

data Sheet a = Sheet { name::String, focus::Ref, cells::Array Ref a} deriving (Eq)

instance Functor Sheet where
    fmap f (Sheet n r xss) =  Sheet n r $ fmap f xss


instance Comonad Sheet where
	extract (Sheet n ix css) = css Data.Array.! ix
	duplicate (Sheet n ix css) = Sheet n ix $ listArray (bounds css) $ fmap (\jx-> Sheet n jx css) $ indices css

shift :: Ref -> Sheet a -> Sheet a
shift r1 (Sheet n r2 ass) = Sheet n (refAdd (snd $ bounds ass) r1 r2) ass

instance (Show e) => Show (Sheet e) where
    show ss = printf $ concat $ intersperse "\n" $ elems $ fmap show $ cells ss

 
(!)::Sheet a ->Ref-> a
(!) s r =  (cells s)Data.Array.!r
 
-- data Sheet2 s a = Sheet2 { name2::String, focus2::Ref, cells2::ST s (STArray s Ref a)}

{-
--instance Functor (Sheet2 s) where
--    fmap f (Sheet2 n r xss) =  Sheet2 n r $ fmap f $ runST xss

buildPair' :: Array Int Int
buildPair' = runSTArray buildPair

buildPair :: ST s (STArray s Int Int)
buildPair = do arr <- newArray (1,10) 37 :: ST s (STArray s Int Int)
               a <- readArray arr 1
               writeArray arr 1 64
               b <- readArray arr 1
               return arr
 
-}



