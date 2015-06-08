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

--module SheetTest (e1, e2, e3, v1, v2, v3, e, val, vat, sheet2, sheet3, sheet4, sheet5, wfixTest
--                    , wfixTest2, wfixTest3, wfixTest4, wfixTest5, r4, cr4, v4, setBlur, UI.setFocus, setup, getCell, renderCell, ee) where

import Data.Array
import Data.Map as M hiding (map, foldl, foldl',  (!))
import Data.Maybe
import Data.List
import Data.Monoid
import Data.Either
import Control.Monad
import Control.Monad.Identity
import Control.Comonad hiding ((<@))
import Text.Printf
import Data.Either.Utils (fromRight)
import Text.PrettyPrint hiding ((<>))
import Data.Char
import Text.ParserCombinators.Parsec as P hiding (string)
import Safe (readMay)
import Data.IORef

import qualified Control.Monad.State as S
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core as C

import Control.Concurrent.STM.TVar
import Control.Concurrent.STM

import Cat
import Ref
import Expr
import Sheet
import Cell
import Parser

main :: IO ()
main = do 
    --theSpreadsheet <- atomically $ newTVar spreadsheet    
    rSheet <- newIORef sheet5
    let refIn = fromCoords (1,1)
    startGUI defaultConfig $ setup rSheet

ncols::Int
ncols= x $ cRef $ snd $ bounds $ cells $ sheet5

nrows::Int
nrows= y $ rRef $ snd $ bounds $ cells $ sheet5

setup sheet window = void $ do 
    setup' sheet window

setup' :: IORef (Sheet CellFn)-> Window -> UI Element
setup' rSheet window = do   
    set title "Spreadsheet" $ return window     
    -- cells are elements
    cellFields <- sequence $ map (\row-> 
            sequence $ map (\col-> do
                let ref = Ref (CAbs col) (RAbs row)
                r <- renderInCell rSheet ref
                --rr <- renderMove rSheet ref [[]] r
                return r
            ) [1..ncols]
        ) [1..nrows]

    let
        -- The first row of fields are the column names which are letters
        colnames = (C.string ""):[C.string $ [chr $ ord 'A' + c - 1]|c<-[1..ncols]]
        -- The first field is just the row name - which is an integer
        rows = zipWith (\r rowCells -> (C.string $ show r) : map element rowCells) [1..nrows] cellFields

    -- Put the cells on the window - the cells are children of the window
    getBody window #+ [ grid [[grid $ colnames:rows]]]


-- This function redisplays the sheet from the sheet IORef
recalcSheet :: IORef (Sheet CellFn) -> [Element] -> UI [Element]
recalcSheet rSheet fields = do
    sheet <- liftIO (readIORef rSheet)
    mapM (\c-> display sheet c) fields
    --return ()

display :: (Sheet CellFn) -> Element -> UI Element
display sheet cell = return cell


-- Sets a cell to blurred - copied from UI.setFocus
setBlur :: Element -> UI ()
setBlur = runFunction . ffi "$(%1).blur()"

-- Gets a cell by id (which is show r)
getCell :: Ref -> UI Element
getCell r =  do
    w <- askWindow    
    e <- getElementById w $ show r
    return $ fromJust e


-- Renders a cell
-- A cell needs to update the sheet when new data is put in
-- It should display the input string when on focus, the value when not in focus
-- It should change value when a referenced cell changes value
-- This means that the updates should work like wfix?
renderInCell :: IORef (Sheet CellFn) -> Ref -> UI Element
renderInCell rSheet refIn = do

    debug "In renderInCell"
    -- Get the sheet out of the IORef and get the sheet bounds for the navigation
    sheet <- liftIO $ readIORef rSheet

    -- First work out the cell id
    let (x,y)= toCoords refIn
        tabIndex = x * nrows + y

    -- This makes an input element and sets the id to be the ref    
    input  <- UI.input # set (attr "tabindex") (show tabIndex)  # set UI.id_ (show refIn)


    -- Try the on version first...
    on UI.valueChange input $ const $ void $ do  
        val <- get UI.value input
        let newSheet = parseToSheet sheet refIn val
        liftIO $ writeIORef rSheet newSheet

    -- Try the on version first...
    on UI.focus input $ const $ void $ do  
        val <- get UI.value input
        let newSheet = parseToSheet sheet refIn val
        liftIO $ writeIORef rSheet newSheet

    let bs = snd $ bounds $ cells $ sheet
        move :: Ref -> UI ()
        move direction = do
            setBlur input
            next <- getCell $ refAdd bs refIn direction
            UI.setFocus next

        -- Try the on version first...
        setValue :: UI Element
        setValue = do  
            val <- get UI.value input
            newSheet <- liftIO $ readIORef rSheet
            let outValue = parseToValue newSheet refIn val
            element input # set value outValue


    -- If an arrow key or return are pressed blur the current cell
    on UI.keydown input $ \c ->case c of
                                37 -> do 
                                        setValue
                                        move rLeft
                                38 -> do 
                                        setValue
                                        move rUp
                                39 -> do 
                                        setValue
                                        move rRight
                                40 -> do 
                                        setValue
                                        move rDown
                                13 -> do 
                                        setValue
                                        move rDown
                                _ -> return ()        
    return input

renderMove :: IORef (Sheet CellFn) -> Ref -> [[Element]] -> Element -> UI Element
renderMove rSheet refIn eCells input = do
    sheet <- liftIO $ readIORef rSheet
    let bs = snd $ bounds $ cells $ sheet
        move :: Ref -> UI ()
        move direction = do
            setBlur input
            next <- getCell $ refAdd bs refIn direction
            UI.setFocus next

        -- Try the on version first...
        setValue :: UI Element
        setValue = do  
            val <- get UI.value input
            newSheet <- liftIO $ readIORef rSheet
            let outValue = parseToValue newSheet refIn val
            element input # set value outValue
{-
    on UI.keydown input $ \c ->case c of
                                37 -> do 
                                        setValue
                                        move rLeft
                                38 -> do 
                                        setValue
                                        move rUp
                                39 -> do 
                                        setValue
                                        move rRight
                                40 -> do 
                                        setValue
                                        move rDown
                                13 -> do 
                                        setValue
                                        move rDown
                                _ -> return ()          
-}
    return input


{-
We should have an edit cell which shows the input string of the cell in focus while the cell in focus
continues to show the evaluated cell

Talking about replacing cells in the sheet.
We get the string which we can parse to a CellFn.
We need to add this CellFn into the sheet before we do the wfix (which converts all the CellFns to Fix Cells)

-}


-- Parses a string (from a cell input)
-- The parsedInput is an Either CellFn

-- This is what my sheets are
-- newSheet :: Sheet CellFn

-- wfix evaluates the all the cells to Fix Cells
-- newSheet >== wfix :: Sheet FixCell
 
 -- Putting the $ on the from turns is into a function 
-- ($ newSheet =>> wfix) :: (Sheet (Fix Cell) -> b) -> b

-- which is the same as:
-- ($ newSheet =>> wfix) :: CellFn -> Fix Cell

-- fmap maps this function over the either
-- fmap ( $ newSheet =>> wfix) :: f (Sheet (Fix Cell) -> b) -> f b

-- So we get:
-- fmap ( $ newSheet =>> wfix) parsedInput ::  Either ParseError (Fix Cell)

-- Now the second fmap just turns this Fix Cell into a Value and then prints it
-- and the either just changes tjhe error message

-- The upshot is that the sheet doesn't change, but the new sheet is all evaluated and then thrown away!
-- Just the actual cell is returned, none of the cells that depend on it will change because the 
-- original sheet stays the same.
-- Could we return the new sheet instead?
parseToValue:: Sheet CellFn -> Ref -> String-> String
parseToValue sheet ref cellInput = either (const "Parse error") id $ fmap (show . runId . eval) $ fmap ($ newSheet =>> wfix) parsedInput
    where
        parsedInput = parse expr "" cellInput
        -- Parse the input to a CellFn and put it in the sheet
        newCells = (cells sheet)//[(ref,either (const $ sval "Parse error") id parsedInput)]
        -- Make a Sheet with the new cells
        newSheet = Sheet (name sheet) (focus sheet) newCells

-- This parses the same but it returns the new sheet
parseToSheet:: Sheet CellFn -> Ref -> String -> Sheet CellFn
parseToSheet sheet ref cellInput = Sheet (name sheet) (focus sheet) newCells
    where
        parsedInput = parse expr "" cellInput
        -- Parse the input to a CellFn and put it in the sheet
        newCells = (cells sheet)//[(ref,either (const $ sval "Parse error") id parsedInput)]


{-************************************************************************************************************

-- Some tests of this stuff...

************************************************************************************************************-}

sheet5 :: Sheet CellFn
sheet5 = Sheet "CellFn5" (fromCoords (1,1)) $ listArray (fromCoords (1,1), fromCoords (5,5)) $ all
	where
		row0::[CellFn]
		row0 = [ (nval 1.5)
			, (nval 1.0)
			-- , \ss -> Fix $ CA $ NRef ss $ fromCoords(1,2)
			, eref $ fromCoords (2,3)
			, eref $ fromCoords (3,2)
			, eref $ fromCoords (2,2)]
		row1::[CellFn]
		row1 = [ (nval 10.0)
			, (nval 7.0)
			, eref $ fromCoords (2,2)
			, eref $ fromCoords (4,2)
			, eadd (eref $ fromCoords (2,1)) (nval 41.0)]
		row2::[CellFn]
		row2 = [ (nval 20.0) 
			, (nval 21.0)
			, eref $ fromCoords (2,3)
			, eref $ fromCoords (2,2)
			, emul (eref $ fromCoords (2,2)) (nval 43.0)]
		row3::[CellFn]
		row3 = [ (nval 30.0) 
			, econcat (sval "Hello") (sval ", Nick")
			, eref $ fromCoords (2,3)
			, eref $ fromCoords (1,2)
			, ediv (eref $ fromCoords (1,1)) (nval 45.0)]
		row4::[CellFn]
		row4 = [ (sval "Nick")
			, eor (bval False) (bval True)
			, eref $ fromCoords (5,1)
			, eref $ fromCoords (5,2)
			, eref $ fromCoords (2,2)]
		all = concat $ [row0, row1, row2, row3, row4]



oldPrintSheet :: Sheet Double -> IO ()
oldPrintSheet ss =
      forM_ [0..4] $ \i -> do
            forM_ [0..4] $ \j ->
                  printf "%4.1f   " (ass ! (fromCoords (i,j)))
            printf "\n"
	where
		ass = cells ss		

printVSheet :: Sheet Value -> IO ()
printVSheet ss =
      forM_ [0..4] $ \i -> do
            forM_ [0..4] $ \j ->
                  printf "%s   " (show $ ass ! (fromCoords (i,j)))
            printf "\n"
	where
		ass = cells ss		

printFSheet :: Sheet (Fix Cell) -> IO ()
printFSheet ss =
      forM_ [0..4] $ \i -> do
            forM_ [0..4] $ \j ->
                  printf "%s   " (show $ ass ! (fromCoords (i,j)))
            printf "\n"
	where
		ass = cells ss		


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
vat ix ss = (ass ! ix)*0.2 where ass = cells ss

-- Sum of the values at a list of indices
sum' :: Num a => [Ref] -> Sheet a -> a
sum' ixs = \ss -> foldl' (\acc ix -> acc + (cells ss) ! ix) 0 ixs


sheet1 :: Sheet (Sheet Double -> Double)
sheet1 = Sheet "Double" (fromCoords (0,0)) $ listArray (fromCoords (0,0), fromCoords (4,4))
--      Prices | VAT        | Effective prices + total
      [ val 1, vat $ fromCoords (0,0), sum' [fromCoords (0,i) | i <- [0..1]], (\ss -> (cells ss) ! (fromCoords (0,0)) + (cells ss) ! (fromCoords (0,1))), e
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
			, \ss-> (cells ss)!(fromCoords (1,1))*47.0]
		row1 = [ const 10.0 
			, const 11.0
			, \ss -> coreturn $ shift (fromCoords (1,2)) ss
			, \ss -> coreturn $ shift (fromCoords (3,1)) ss
			, \ss-> (cells ss)!(fromCoords (1,1))*47.0]
		row2 = [ const 20.0 
			, const 21.0
			, \ss -> coreturn $ shift (fromCoords (1,2)) ss
			, \ss -> coreturn $ shift (fromCoords (-1,-1)) ss
			, \ss-> (cells ss)!(fromCoords (1,1))*47.0]
		row3 = [ const 30.0 
			, const 31.0 
			, \ss -> coreturn $ shift (fromCoords (1,2)) ss
			, \ss -> coreturn $ shift (fromCoords (0,1)) ss
			, \ss-> (cells ss)!(fromCoords (1,1))*47.0]
		row4 = [ const 40.0
			, const 41.0
			, \ss -> coreturn $ shift (fromCoords (-1,-1)) ss
			, \ss -> coreturn $ shift (fromCoords (-1,1)) ss
			, \ss-> (cells ss)!(fromCoords (1,1))*47.0]
		all = concat $ [row0, row1, row2, row3, row4]

sheet3 :: Sheet (Sheet Value -> Value)
sheet3 = Sheet "Value" (fromCoords (1,1)) $ listArray (fromCoords (0,0), fromCoords (4,4)) $ all
	where
		row0::[Sheet Value-> Value]
		row0 = [ const (N 1.5)
			, const (N 1.0)
			, \ss -> coreturn $ shift (fromCoords (1,2)) ss
			, \ss -> coreturn $ shift (fromCoords (2,1)) ss
			, \ss-> (cells ss)!(fromCoords (1,1))*47.0]
		row1 = [ const 10.0
			, const 7.0
			, \ss -> coreturn $ shift (fromCoords (1,2)) ss
			, \ss -> coreturn $ shift (fromCoords (3,1)) ss
			, \ss-> (cells ss)!(fromCoords (1,1))*47.0]
		row2 = [ const 20.0 
			, const 21.0
			, \ss -> coreturn $ shift (fromCoords (1,2)) ss
			, \ss -> coreturn $ shift (fromCoords (-1,-1)) ss
			, \ss-> (cells ss)!(fromCoords (1,1))*47.0]
		row3 = [ const 30.0 
			, const 31.0 
			, \ss -> coreturn $ shift (fromCoords (1,2)) ss
			, \ss -> coreturn $ shift (fromCoords (0,1)) ss
			, \ss-> (cells ss)!(fromCoords (0,0))*47.0]
		row4 = [ const (S "Nick")
			, const (B False)
			, \ss -> coreturn $ shift (fromCoords (0,-2)) ss
			, \ss -> coreturn $ shift (fromCoords (-1,1)) ss
			, \ss-> (cells ss)!(fromCoords (1,1))*47.0]
		all = concat $ [row0, row1, row2, row3, row4]


sheet4 :: Sheet CellFn
sheet4 = Sheet "CellFn" (fromCoords (2,2)) $ listArray (fromCoords (0,0), fromCoords (4,4)) $ all
	where
		row0::[Sheet (Fix Cell) -> Fix Cell]
		row0 = [ (nval 1.5)
			, (nval 1.0)
			, \ss -> coreturn $ shift (fromCoords (1,2)) ss
			, \ss -> coreturn $ shift (fromCoords (2,1)) ss
			, \ss-> (cells ss)!(fromCoords (1,1))]
		row1::[Sheet (Fix Cell) -> Fix Cell]
		row1 = [ (nval 10.0)
			, (nval 7.0)
			, \ss -> coreturn $ shift (fromCoords (1,2)) ss
			, \ss -> coreturn $ shift (fromCoords (3,1)) ss
			, eadd (\ss -> (cells ss)!(fromCoords (1,1))) (nval 41.0)]
		row2::[Sheet (Fix Cell) -> Fix Cell]
		row2 = [ (nval 20.0) 
			, (nval 21.0)
			, \ss -> coreturn $ shift (fromCoords (1,2)) ss
			, \ss -> coreturn $ shift (fromCoords (-1,-1)) ss
			, emul (\ss -> (cells ss)!(fromCoords (1,1))) (nval 43.0)]
		row3::[Sheet (Fix Cell) -> Fix Cell]
		row3 = [ (nval 30.0) 
			, econcat (sval "Hello") (sval ", Nick")
			, \ss -> coreturn $ shift (fromCoords (1,2)) ss
			, \ss -> coreturn $ shift (fromCoords (0,1)) ss
			, ediv (\ss -> (cells ss)!(fromCoords (0,0))) (nval 45.0)]
		row4::[Sheet (Fix Cell) -> Fix Cell]
		row4 = [ (sval "Nick")
			, eor (bval False) (bval True)
			, \ss -> coreturn $ shift (fromCoords (0,-2)) ss
			, \ss -> coreturn $ shift (fromCoords (-1,1)) ss
			, \ss-> (cells ss)!(fromCoords (1,1))]
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


