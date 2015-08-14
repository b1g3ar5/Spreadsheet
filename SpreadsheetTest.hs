{-# LANGUAGE NoMonomorphismRestriction #-}

module SpreadsheetTest (sheetString, sheetString2, e1, e2, e3, v1, v2, v3, e, val, vat, sheet2, sheet3, sheet4, sheet5, wfixTest
                    , wfixTest2, wfixTest3, wfixTest4, wfixTest5, r4, cr4, v4) where

import Data.Array hiding ((!), (//))
import Data.List
import Control.Monad.Identity
import Control.Comonad hiding ((<@))
import Text.Printf

import qualified Control.Monad.State as S
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core as C
import Text.ParserCombinators.Parsec as P hiding (string)

import Cat
import Ref
import Expr
import Sheet
import Cell
import Parser
import RefParser


{-
let str = "=A3"
let ref = readRef "B2"
let ss = sSimple
let ptest = parseRefsSheet ss
let ltest = loeb $ ptest//[(ref, bval True)]
let f = either (const $ sval "Parse error") id $ parse refexpr "" str
-}

-- Returns the parsed input string (ie. a CellFn) applied to the sheet of True/Falses
-- which show the cells that depend on the ref
-- The parsed CellFn just just cfor's all the bools together
isCirc :: String -> Ref -> Sheet String -> Bool
isCirc str ref ss = (runId $ eval $ f ltest) == (B True)
    where
        -- Parse sheet to functions whcih return True/False - whether it depends on ref
        ptest :: Sheet CellFn
        ptest = parseRefsSheet ss
        -- Apply the CellFns - with ref cell replaced by True
        ltest :: Sheet (Fix Cell)
        ltest = loeb $ ptest//[(ref, bval True)]
        -- Parse the input string
        f :: CellFn
        f = either (const $ sval "Parse error") id $ parse refexpr "" str

-- Returns the parsed input string (ie. a CellFn) applied to the sheet of True/Falses
-- which show the cells that depend on the ref
-- The parsed CellFn just just cfor's all the bools together
testCirc :: String -> Ref -> Sheet String -> Fix Cell
testCirc str ref ss = f ltest
    where
        -- Parse sheet to functions whcih return True/False - whether it depends on ref
        ptest :: Sheet CellFn
        ptest = parseRefsSheet ss
        -- Apply the CellFns - with ref cell replaced by True
        ltest :: Sheet (Fix Cell)
        ltest = loeb $ ptest//[(ref, bval True)]
        -- Parse the input string
        f :: CellFn
        f = either (const $ sval "Parse error") id $ parse refexpr "" str

nSimple :: Sheet CellFn
nSimple = Sheet "Simple" (fromCoords (1,1)) $ listArray (fromCoords (1,1), fromCoords (3,3)) $ all
	where
		colA::[CellFn]
		colA = [ nval 1.0
			, nval 2.0
			, nval 3.0]
		colB::[CellFn]
		colB = [ nval 4.0
			, nval 5.0
			, nval 6.0]
		colC::[CellFn]
		colC = [ nval 7.0
			, nval 8.0
			, nval 9.0]
		all = concat $ [colA, colB, colC]

bSimple :: Sheet CellFn
bSimple = Sheet "Simple" (fromCoords (1,1)) $ listArray (fromCoords (1,1), fromCoords (3,3)) $ all
	where
		colA::[CellFn]
		colA = [ bval False
			, bval False
			, bval False]
		colB::[CellFn]
		colB = [ bval False
			, bval False
			, bval False]
		colC::[CellFn]
		colC = [ bval False
			, bval False
			, bval False]
		all = concat $ [colA, colB, colC]


sSimple :: Sheet String
sSimple = Sheet "Simple" (fromCoords (1,1)) $ listArray (fromCoords (1,1), fromCoords (3,3)) $ all
	where
		colA::[String]
		colA = [ "=1.0"
			, "=2.0"
			, "=3.0"]
		colB::[String]
		colB = [ "=a1"
			, "=5.0"
			, "=6.0"]
		colC::[String]
		colC = [ "=a1"
			, "=8.0"
			, "=9.0"]
		all = concat $ [colA, colB, colC]






{-************************************************************************************************************

-- Some tests of this stuff...

************************************************************************************************************-}

sheetString :: Sheet String
sheetString = Sheet "CellFn5" (fromCoords (1,1)) $ listArray (fromCoords (1,1), fromCoords (5,15)) $ all
	where
		colA::[String]
		colA = [ "=\"\""
			, "=\"Future\""
			, "=\"Strike\""
			, "=\"Volatility\""
			, "=\"Rate\""
			, "=\"Time\""
			, "=\"\""
			, "=\"Call\""
			, "=\"Put\""
			, "=\"CallDelta\""
			, "=\"PutDelta\""
			, "=\"Gamma\""
			, "=\"Vega\""
			, "=\"d1\""
			, "=\"d2\""]
		colB::[String]
		colB = [ "=\"Option\""
			, "=100"
			, "=90"
			, "=0.1"
			, "=0.0"
			, "=1.0"
			, "=\"\""
			, "=exp(0-b5*b6)*(b2*pnorm(b14)-b3*pnorm(b15))"
			, "=exp(0-b5*b6)*(b3-b2)+b8"
			, "=exp(0-b5*b6)*pnorm(b14)"
			, "=0.0+b10-exp(0-b5*b6)"
			, "=exp(0-b5*b6)*dnorm(b14)/b2/b4/b6^0.5"
			, "=b12*bs*bs*b4*b6"
			, "=log(b2/b3)/b4/b6^0.5+b4*b6^0.5/2"
			, "=log(b2/b3)/b4/b6^0.5-b4*b6^0.5/2"]

		colC::[String]
		colC = [ "=\"Option\""
			, "=0.0+b2+e2"
			, "=b3"
			, "=b4"
			, "=b5"
			, "=b6"
			, "=\"\""
			, "=exp(0-c5*c6)*(c2*pnorm(c14)-c3*pnorm(c15))"
			, "=exp(0-c5*c6)*(c3-c2)+c8"
			, "=exp(0-c5*c6)*pnorm(c14)"
			, "=0.0+c10-exp(0-c5*c6)"
			, "=exp(0-c5*c6)*dnorm(c14)/c2/c4/c6^0.5"
			, "=c12*c2*c2*c4*c6"
			, "=log(c2/c3)/c4/c6^0.5+c4*c6^0.5/2"
			, "=log(c2/c3)/c4/c6^0.5-c4*c6^0.5/2"]
		colD::[String]
		colD = ["=\"Option\""
			, "=0.0+b2-e2"
			, "=b3"
			, "=b4"
			, "=b5"
			, "=b6"
			, "=\"\""
			, "=exp(0-d5*d6)*(d2*pnorm(d14)-d3*pnorm(d15))"
			, "=exp(0-d5*d6)*(d3-d2)+d8"
			, "=exp(0-d5*d6)*pnorm(d14)"
			, "=0.0+d10-exp(0-d5*d6)"
			, "=exp(0-d5*d6)*dnorm(d14)/d2/d4/d6^0.5"
			, "=d12*d2*d2*d4*d6"
			, "=log(d2/d3)/d4/d6^0.5+d4*d6^0.5/2"
			, "=log(d2/d3)/d4/d6^0.5-d4*d6^0.5/2"]
		colE::[String]
		colE = [ "=\"Increment\""
			, "=0.00001"
			, "=\"\""
			, "=\"\""
			, "=\"\""
			, "=\"\""
			, "=\"Check Derivs\""
			, "=(c8-d8)/e2/2"
			, "=(c9-d9)/e2/2"
			, "=(c10-d10)/e2/2"
			, "=(c11-d11)/e2/2"
			, "=log(10)"
			, "=addmonths(10,34567)"
			, "=\"\""
			, "=\"\""]
		all = concat $ [colA, colB, colC, colD, colE]

sheetString3 :: Sheet String
sheetString3 = Sheet "CellFn5" (fromCoords (1,1)) $ listArray (fromCoords (1,1), fromCoords (5,5)) $ all
	where
		colA::[String]
		colA = [ "=1"
			, "=2"
			, "=3"
			, "=4"
			, "=5"]
		colB::[String]
		colB = [ "=1+a1"
			, "=2+a2"
			, "=3+a3"
			, "=4+a4"
			, "=5+a5"]
		colC::[String]
		colC = [ "=11+b1"
			, "=12+b2"
			, "=13+B3"
			, "=14+b4"
			, "=15+b5"]
		colD::[String]
		colD = [ "=21+c1"
			, "=22+c2"
			, "=23+c3"
			, "=24+c4"
			, "=25+c5"]
		colE::[String]
		colE = [ "=31+d1"
			, "=32+d2"
			, "=33+d2"
			, "=34+d4"
			, "=35+d5"]
		all = concat $ [colA, colB, colC, colD, colE]


sheetString2 :: Sheet String
sheetString2 = Sheet "CellFn5" (fromCoords (1,1)) $ listArray (fromCoords (1,1), fromCoords (5,5)) $ all
	where
		colA::[String]
		colA = [ "=1.5"
			, "=1"
			, "=b3"
			, "=C2"
			, "=B2"]
		colB::[String]
		colB = [ "=10.0"
			, "=7.0"
			, "=B2"
			, "=D2"
			, "=b1+41"]
		colC::[String]
		colC = [ "=20"
			, "=21"
			, "=B3"
			, "=b2"
			, "=b2*43"]
		colD::[String]
		colD = [ "=30.0"
			, "=\"Hello\"&\" Nick\""
			, "=B3"
			, "=A2"
			, "=A1/45"]
		colE::[String]
		colE = [ "=\"Nick\""
		    , "=False||True"
			, "=E1"
			, "=E2"
			, "=B2"]
		all = concat $ [colA, colB, colC, colD, colE]

sheetStringCirc :: Sheet String
sheetStringCirc = Sheet "CellFn5" (fromCoords (1,1)) $ listArray (fromCoords (1,1), fromCoords (5,5)) $ all
	where
		colA::[String]
		colA = [ "=1.5"
			, "=1"
			, "=b3"
			, "=C2"
			, "=B2"]
		colB::[String]
		colB = [ "=10.0"
			, "=a3"
			, "=B2"
			, "=D2"
			, "=b1+41"]
		colC::[String]
		colC = [ "=20"
			, "=21"
			, "=B3"
			, "=b2"
			, "=b2*43"]
		colD::[String]
		colD = [ "=30.0"
			, "=\"Hello\"&\" Nick\""
			, "=B3"
			, "=A2"
			, "=A1/45"]
		colE::[String]
		colE = [ "=\"Nick\""
		    , "=False||True"
			, "=E1"
			, "=E2"
			, "=B2"]
		all = concat $ [colA, colB, colC, colD, colE]

sheetStringA3 :: Sheet String
sheetStringA3 = Sheet "CellFn5" (fromCoords (1,1)) $ listArray (fromCoords (1,1), fromCoords (5,5)) $ all
	where
		colA::[String]
		colA = [ "=False"
			, "=False"
			, "=True"
			, "=False"
			, "=False"]
		colB::[String]
		colB = [ "=False"
			, "=False"
			, "=False"
			, "=False"
			, "=False"]
		colC::[String]
		colC = [ "=False"
			, "=False"
			, "=False"
			, "=False"
			, "=False"]
		colD::[String]
		colD = [ "=False"
			, "=False"
			, "=False"
			, "=False"
			, "=False"]
		colE::[String]
		colE = [ "=False"
		    , "=False"
			, "=False"
			, "=False"
			, "=False"]
		all = concat $ [colA, colB, colC, colD, colE]

sheet5 :: Sheet CellFn
sheet5 = Sheet "CellFn5" (fromCoords (1,1)) $ listArray (fromCoords (1,1), fromCoords (5,5)) $ all
	where
		colA::[CellFn]
		colA = [ (nval 1.5)
			, (nval 1.0)
			, eref $ fromCoords (2,3) -- ie. B3 = B2 = 7
			, eref $ fromCoords (3,2) -- ie. C2 = 21
			, eref $ fromCoords (2,2)] -- ie. B2 = 7
		colB::[CellFn]
		colB = [ (nval 10.0)
			, (nval 7.0)
			, eref $ fromCoords (2,2) -- B2 = 7
			, eref $ fromCoords (4,2) -- D2 = Hello, Nick
			, eadd (eref $ fromCoords (2,1)) (nval 41.0)] -- B1 + 41 = 10 + 41 = 51
		colC::[CellFn]
		colC = [ (nval 20.0) 
			, (nval 21.0)
			, eref $ fromCoords (2,3) -- B3 = B2 = 7
			, eref $ fromCoords (2,2) -- B2 = 7
			, emul (eref $ fromCoords (2,2)) (nval 43.0)] -- B2*43 = 301
		colD::[CellFn]
		colD = [ (nval 30.0) 
			, econcat (sval "Hello") (sval ", Nick")
			, eref $ fromCoords (2,3) -- B3 = B2 = 7
			, eref $ fromCoords (1,2) -- A2 = 1
			, ediv (eref $ fromCoords (1,1)) (nval 45.0)] -- A1 / 45 = 1.5/45 = 1/30 = 0.0333
		colE::[CellFn]
		colE = [ (sval "Nick")
			, eor (bval False) (bval True)
			, eref $ fromCoords (5,1) -- E1 = "Nick"
			, eref $ fromCoords (5,2) -- E2 = True
			, eref $ fromCoords (2,2)] -- B2 = 7
		all = concat $ [colA, colB, colC, colD, colE]

sheet6 :: Sheet CellFn
sheet6 = Sheet "CellFn6" (fromCoords (1,1)) $ listArray (fromCoords (1,1), fromCoords (5,5)) $ all
	where
		colA::[CellFn]
		colA = [ (nval 1.5)
			, (nval 1.0)
			, eref $ fromCoords (2,3) -- ie. B3 = B2 = 7
			, eref $ fromCoords (3,2) -- ie. C2 = 21
			, eref $ fromCoords (2,2)] -- ie. B2 = 7
		colB::[CellFn]
		colB = [ (nval 10.0)
			, (nval 7.0)
			, eref $ fromCoords (2,2) -- B2 = 7
			, eref $ fromCoords (4,2) -- D2 = Hello, Nick
			, eadd (eref $ fromCoords (2,1)) (nval 41.0)] -- B1 + 41 = 10 + 41 = 51
		colC::[CellFn]
		colC = [ (nval 20.0) 
			, (nval 21.0)
			, eref $ fromCoords (2,3) -- B3 = B2 = 7
			, eref $ fromCoords (2,2) -- B2 = 7
			, emul (eref $ fromCoords (2,2)) (nval 43.0)] -- B2*43 = 301
		colD::[CellFn]
		colD = [ (nval 30.0) 
			, econcat (sval "Hello") (sval ", Nick")
			, eref $ fromCoords (2,3) -- B3 = B2 = 7
			, eref $ fromCoords (1,2) -- A2 = 1
			, ediv (eref $ fromCoords (1,1)) (nval 45.0)] -- A1 / 45 = 1.5/45 = 1/30 = 0.0333
		colE::[CellFn]
		colE = [ (sval "Nick")
			, eor (bval False) (bval True)
			, eref $ fromCoords (5,1) -- E1 = "Nick"
			, eref $ fromCoords (5,2) -- E2 = True
			, eref $ fromCoords (2,2)] -- B2 = 7
		all = concat $ [colA, colB, colC, colD, colE]



oldPrintSheet :: Sheet Double -> IO ()
oldPrintSheet ss =
      forM_ [0..4] $ \i -> do
            forM_ [0..4] $ \j ->
                  printf "%4.1f   " (ss ! (fromCoords (i,j)))
            printf "\n"

printVSheet :: Sheet Value -> IO ()
printVSheet ss =
      forM_ [0..4] $ \i -> do
            forM_ [0..4] $ \j ->
                  printf "%s   " (show $ ss ! (fromCoords (i,j)))
            printf "\n"

printFSheet :: Sheet (Fix Cell) -> IO ()
printFSheet ss =
      forM_ [0..4] $ \i -> do
            forM_ [0..4] $ \j ->
                  printf "%s   " (show $ ss ! (fromCoords (i,j)))
            printf "\n"


printSheet :: Sheet CellFn -> IO ()
printSheet ss = printCalcedSheet $ ss =>> wfix
{-
      forM_ [0..4] $ \i -> do
            forM_ [0..4] $ \j ->
                  printf "%s   " (show $ wss ! (fromCoords (i,j)))
            printf "\n"
	where
		--ass = cells ss		
		--vss :: Array Ref (Id Value)
		--vss = fmap eval $ cells ss		
		wss = fmap (runId.eval) $ cells ss		
		--xss = fmap (show.runId.eval) $ cells ss		
-}


loebTest = oldPrintSheet $ loeb sheet1

wfixTest = oldPrintSheet $ sheet1 =>> wfix

wfixTest2 = oldPrintSheet $ sheet2 =>> wfix
wfixTest3 = printVSheet $ sheet3 =>> wfix

-- wfix converts a Sheet CellFn to Sheet (Fix Cell)
-- But all the references need to be converted to functions on the sheet
-- So if we parse using nRef:: Parser (Fix Cell) which call enref we get
-- (NRef emptySheet Ref) in all the expressions.
-- Now fmap cellConvert will not work because it doesn't nore down into the expressions
-- We need something like evalArg
wfixTest4 = printCalcedSheet $ sheet4 =>> wfix
wfixTest5 = printCalcedSheet $ sheet5 =>> wfix



areTheyEqual = (loeb sheet1) == (sheet1 =>> wfix)


{-------------------------------------------------------------------------------------------------------------

CODE GRAVEYARD

--------------------------------------------------------------------------------------------------------------}

e1 :: CellFn
e1 = nval 18 `eadd` nval 24

e2 :: CellFn
e2 = bval True `eand` bval False

e3 :: CellFn
e3 = sval "Nick" `econcat` sval " Straw"

v1 :: Either String Value
v1 = eval $ e1 $ sheet4 =>> wfix

v2 :: Maybe Value
v2 = eval $ e2 $ sheet4 =>> wfix

v3 = eval $ e3 $ sheet4 =>> wfix

r4 = eref $ fromCoords(1,1)

cr4 = eref $ fromCoords(1,1)

v4 :: Sheet (Fix Cell)
v4 = sheet4 =>> wfix

-- Empty cell
e = val 0

-- Simple cell value
val = const

-- VAT of a cell's contents (10 %)
--vat :: Ref -> Array Ref Double -> Double
vat :: Ref -> Sheet Double -> Double
vat ix ss = (ss ! ix)*0.2 

-- Sum of the values at a list of indices
sum' :: Num a => [Ref] -> Sheet a -> a
sum' ixs = \ss -> foldl' (\acc ix -> acc + ss!ix) 0 ixs


sheet1 :: Sheet (Sheet Double -> Double)
sheet1 = Sheet "Double" (fromCoords (0,0)) $ listArray (fromCoords (0,0), fromCoords (4,4))
--      Prices | VAT        | Effective prices + total
      [ val 1, vat $ fromCoords (0,0), sum' [fromCoords (0,i) | i <- [0..1]], (\ss -> ss!(fromCoords (0,0)) + ss!(fromCoords (0,1))), e
      , val 3, vat $ fromCoords (1,0), sum' [fromCoords (1,i) | i <- [0..1]], e, e
      , val 5, vat $ fromCoords (2,0), sum' [fromCoords (2,i) | i <- [0..1]], e, e
      , val 2, vat $ fromCoords (3,0), sum' [fromCoords (3,i) | i <- [0..1]], e, e
      , sum' [fromCoords (i,0) | i <- [0..3]], sum' [fromCoords (i,1) | i <- [0..3]], sum' [fromCoords (i,2) | i <- [0..3]], e, e
      ]

sheet2 :: Sheet (Sheet Double -> Double)
sheet2 = Sheet "Double2" (fromCoords (1,1)) $ listArray (fromCoords (0,0), fromCoords (4,4)) $ all
	where
		row0 = [ const 0.0
			, const 1.0
			, \ss -> coreturn $ shift (fromCoords (1,2)) ss
			, \ss -> coreturn $ shift (fromCoords (2,1)) ss
			, \ss-> ss!(fromCoords (1,1))*47.0]
		row1 = [ const 10.0 
			, const 11.0
			, \ss -> coreturn $ shift (fromCoords (1,2)) ss
			, \ss -> coreturn $ shift (fromCoords (3,1)) ss
			, \ss-> (ss)!(fromCoords (1,1))*47.0]
		row2 = [ const 20.0 
			, const 21.0
			, \ss -> coreturn $ shift (fromCoords (1,2)) ss
			, \ss -> coreturn $ shift (fromCoords (-1,-1)) ss
			, \ss-> (ss)!(fromCoords (1,1))*47.0]
		row3 = [ const 30.0 
			, const 31.0 
			, \ss -> coreturn $ shift (fromCoords (1,2)) ss
			, \ss -> coreturn $ shift (fromCoords (0,1)) ss
			, \ss-> (ss)!(fromCoords (1,1))*47.0]
		row4 = [ const 40.0
			, const 41.0
			, \ss -> coreturn $ shift (fromCoords (-1,-1)) ss
			, \ss -> coreturn $ shift (fromCoords (-1,1)) ss
			, \ss-> (ss)!(fromCoords (1,1))*47.0]
		all = concat $ [row0, row1, row2, row3, row4]

sheet3 :: Sheet (Sheet Value -> Value)
sheet3 = Sheet "Value" (fromCoords (1,1)) $ listArray (fromCoords (0,0), fromCoords (4,4)) $ all
	where
		row0::[Sheet Value-> Value]
		row0 = [ const (N 1.5)
			, const (N 1.0)
			, \ss -> coreturn $ shift (fromCoords (1,2)) ss
			, \ss -> coreturn $ shift (fromCoords (2,1)) ss
			, \ss-> (ss)!(fromCoords (1,1))*47.0]
		row1 = [ const 10.0
			, const 7.0
			, \ss -> coreturn $ shift (fromCoords (1,2)) ss
			, \ss -> coreturn $ shift (fromCoords (3,1)) ss
			, \ss-> (ss)!(fromCoords (1,1))*47.0]
		row2 = [ const 20.0 
			, const 21.0
			, \ss -> coreturn $ shift (fromCoords (1,2)) ss
			, \ss -> coreturn $ shift (fromCoords (-1,-1)) ss
			, \ss-> (ss)!(fromCoords (1,1))*47.0]
		row3 = [ const 30.0 
			, const 31.0 
			, \ss -> coreturn $ shift (fromCoords (1,2)) ss
			, \ss -> coreturn $ shift (fromCoords (0,1)) ss
			, \ss-> (ss)!(fromCoords (0,0))*47.0]
		row4 = [ const (S "Nick")
			, const (B False)
			, \ss -> coreturn $ shift (fromCoords (0,-2)) ss
			, \ss -> coreturn $ shift (fromCoords (-1,1)) ss
			, \ss-> (ss)!(fromCoords (1,1))*47.0]
		all = concat $ [row0, row1, row2, row3, row4]


sheet4 :: Sheet CellFn
sheet4 = Sheet "CellFn" (fromCoords (2,2)) $ listArray (fromCoords (1,1), fromCoords (5,5)) $ all
	where
		row0::[Sheet (Fix Cell) -> Fix Cell]
		row0 = [ (nval 1.5)
			, (nval 1.0)
			, \ss -> coreturn $ shift (fromCoords (1,2)) ss
			, \ss -> coreturn $ shift (fromCoords (2,1)) ss
			, \ss-> (ss)!(fromCoords (1,1))]
		row1::[Sheet (Fix Cell) -> Fix Cell]
		row1 = [ (nval 10.0)
			, (nval 7.0)
			, \ss -> coreturn $ shift (fromCoords (1,2)) ss
			, \ss -> coreturn $ shift (fromCoords (3,1)) ss
			, eadd (\ss -> (ss)!(fromCoords (1,1))) (nval 41.0)]
		row2::[Sheet (Fix Cell) -> Fix Cell]
		row2 = [ (nval 20.0) 
			, (nval 21.0)
			, \ss -> coreturn $ shift (fromCoords (1,2)) ss
			, \ss -> coreturn $ shift (fromCoords (-1,-1)) ss
			, emul (\ss -> (ss)!(fromCoords (1,1))) (nval 43.0)]
		row3::[Sheet (Fix Cell) -> Fix Cell]
		row3 = [ (nval 30.0) 
			, econcat (sval "Hello") (sval ", Nick")
			, \ss -> coreturn $ shift (fromCoords (1,2)) ss
			, \ss -> coreturn $ shift (fromCoords (0,1)) ss
			, ediv (\ss -> (ss)!(fromCoords (0,0))) (nval 45.0)]
		row4::[Sheet (Fix Cell) -> Fix Cell]
		row4 = [ (sval "Nick")
			, eor (bval False) (bval True)
			, \ss -> coreturn $ shift (fromCoords (0,-2)) ss
			, \ss -> coreturn $ shift (fromCoords (-1,1)) ss
			, \ss-> (ss)!(fromCoords (1,1))]
		all = concat $ [row0, row1, row2, row3, row4]



{-
    -- These are events that give a true when the key is pressed    
    let edit = fmap (\kc-> if (kc==113) then True else False) $ UI.keydown input

    let eLeft = fmap (\kc-> if (kc==37) then rLeft else rZero) $ UI.keydown input
    let eUp   = fmap (\kc->if (kc==38) then rUp else rZero) $ UI.keydown input
    let eRight= fmap (\kc->if (kc==39) then rRight else rZero) $ UI.keydown input
    let eDown = fmap (\kc->if (kc==40) then rDown else rZero) $ UI.keydown input
    
    let eMove = UI.unions [eLeft, eRight, eUp, eDown]
    eValue <- stepper "" $ fmap head $ UI.unions [
        pure "Left" <@ eLeft, 
        pure "Right" <@ eRight, 
        pure "Up" <@ eUp, 
        pure "Down" <@ eDown]

    -- These are behaviours that give a true when the key is pressed    
    bLeft <- stepper 0 $ filterE (\kc->kc==37) $ UI.keydown input
    bUp   <- stepper 0 $ filterE (\kc->kc==38) $ UI.keydown input
    bRight<- stepper 0 $ filterE (\kc->kc==39) $ UI.keydown input
    bDown <- stepper 0 $ filterE (\kc->kc==40) $ UI.keydown input




-}


