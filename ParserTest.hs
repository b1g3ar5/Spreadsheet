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

import System.Environment
import Text.ParserCombinators.Parsec -- hiding (string)
import Control.Applicative ((<*), (<$>), (<*>))
import Control.Monad (liftM)
import Control.Comonad hiding ((<@))
import Data.Array
import Data.Char
import Data.Map as M hiding (map, foldl, (!))
import Data.Maybe
import Data.Either (rights)
import Data.Either.Utils (fromRight)
import Data.List
import Data.Monoid
import Data.Function
import Parser
import Sheet
import Cell
import Expr
import Refs
import Cat


main = do
		putStrLn $ showCparse "=3*1"
		putStrLn $ "Now the sequence\n"
		sequence $ map (putStrLn.(\s -> s ++ ": " ++ showCparse s)) ss
		putStrLn $ "Finished the sequence\n"
		putStrLn $ "Now the cfixed sheet\n"
		printCalcedSheet $ sheet =>> wfix


-- Parse the list of strings to a list of CellFns
-- Note the rights needs to be removed otherwise error cells are just removed!!!
cs :: [CellFn]
cs = rights $ fmap (parse expr "") ss

-- Build a sheet fron the CellFns
sheet :: Sheet CellFn
sheet = Sheet "Sheet" (fromCoords (0,0)) $ listArray (fromCoords (1,1), fromCoords (4,5)) $ cs

-- recalc the Sheet - this reduces to a Sheet of simple Fix Cells
calcedSheet :: Sheet (Fix Cell)
calcedSheet = sheet =>> wfix

-- Apply eval to the Fix Cells into Values so that they can be displayed
vsheet :: Sheet Value
vsheet = (runId.eval) <$> calcedSheet


r::Ref
r = fromCoords(2,2)

showCparse s = show $ fromRight $ parse expr "" s

s1 :: String
s1 = "=3*a1"
cf1 :: CellFn
cf1 = fromRight $ parse expr "" s1
fc1 :: Fix Cell
fc1 = cf1 $ sheet =>> wfix
v1 :: String
v1 = show $ runId $ eval fc1

s2 = "=mean(1,2,3)"
cf2 = fromRight $ parse expr "" s2
fc2 = cf2 $ sheet =>> wfix
v2 = show $ runId $ eval fc2

s3 = "=pnorm(1)"
cf3 = fromRight $ parse expr "" s3
fc3 = cf3 $ sheet =>> wfix
v3 = show $ runId $ eval fc3

ss :: [String]
ss = [	"=3*b2"
		, "=c2"
		, "=100-20.35"

		, "=100-20.35^0.5"
		, "=(100-20.35)^0.5"

		, "=(100*20.35)/2+6"
		, "=1+2*3"
		, "=1*2+3"
		, "=(1+2)*3"
		, "=1>3"

		, "=(1>3)&&(4<6)"
		, "=\"Nick\" & \"Straw\""
		, "=1.5"
		, "=(1<3)&&(4=6)||True&&(\"Nick\"=\"Straw\")"
		, "=True = False"

		, "=True && False"
		, "=pnorm(1)"
		, "=mean(1,2)"
		, "=std(1,2,5.6*3+4)"
		, "=2+3*(std(1,2))^0.5"]


