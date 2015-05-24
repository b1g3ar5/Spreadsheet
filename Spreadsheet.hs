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

--module Spreadsheet (e1, e2, e3, v1, v2, v3, e, val, vat, sheet2, sheet3, sheet4, sheet5, wfixTest
--                    , wfixTest2, wfixTest3, wfixTest4, wfixTest5, r4, cr4, v4, setBlur, UI.setFocus, setup, getCell, renderCell, ee) where

import Data.Array hiding ((!))
import Data.Maybe
import Data.Foldable as F hiding (concatMap)
import Data.List.Split
import Control.Monad.Identity
import Control.Comonad hiding ((<@))
import Data.Char
import Text.ParserCombinators.Parsec as P hiding (string)
import Data.IORef

import qualified Control.Monad.State as S
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core as C

import Control.Concurrent.STM.TVar
import Control.Concurrent.STM

import Cat
import Refs
import Expr
import Sheet
import Cell
import Parser
import SpreadsheetTest


-- If we have these functions
--type RenderFn = Sheet (Fix UI ) -> Fix UI
type RenderFn = Sheet (UI Element ) -> UI Element

-- and maybe, this thing (I can't rmember wht this is for!)
--instance Sheet :<: UI where
--    inj :: Sheet a -> UI a
--    inj s = do return $ (cells s)!(focus s)

-- Then if we have a sheet of input cells (which are plain UI elements with simple behaviour)
-- we need to turn each cell into a RenderFn, then we will have a sheet of RenderFn's

-- wfix will then turn this sheet of RenderFn's into a sheet of UI Elements
-- which we can then just eval?

main :: IO ()
main = do   
    let fileName = "TestSheet.ss"
    sheet <- readSheet fileName
    rSheet <- newIORef (sheet) -- , (flip fmap) sheetString $ \c-> either (const $ sval "Parse error") id $ parse expr "" c )
    startGUI defaultConfig $ setup rSheet

{-
In the bartab exaple the IORef is modified by addInput and removeInput and then the whole lot is redisplayed - ie. getBody
is called again. In my case - because all this faffing about doesn't seem to work, I could try this method.
So, I would make the cells - but they would only cope with in cell behaviour and onMove would modify the IORef and then 
call the full recalc.

Now, can we do the recalc with wfix or loab? We have:

wfix :: w (w a -> a) -> a
( \x -> x >== wfix):: w (w a -> a) -> w a = w CellFn -> w (Fix Cell)

If we can then we want to end up with Sheet Element (or maybe UI Element). This mean that the
CellFn equivalent will be 

RenderFn :: w Element -> Element

and we need a w of these. Now we have a [[Element]] returned from all the call to makeCell
but this is no good because [] is nor a comonad

Now, let's just think. Our parser expr takes the string in a cell and makes a CellFn. We can do this for
all our sheet so we use:

fmap (expr ...) :: Sheet String -> Sheet CellFn

thne we have loeb

loeb:: Sheet CellFn -> Sheet (Fix Cell)

which we can then turn back into a sheet of the calulated values

fmap (show.runId.eval) :: Sheet (Fix Cell) -> Sheet String

So the whole lot is

fmap (show.runId.eval) $ loeb $ fmap ((either (const $ sval "Parse error") id).(parse expr "")) $ inputSheet
where
    inputSheet = Sheet of the user input strings

Now, could we have Elements (ie. input cells) on the input sheet?
We could use:

get UI.value cell

to get the value to give to the parser and:

set UI.value str cell

to set the value - with all that apparatus in the middle.

For this to work we need the input sheet always to have the up to date values of each cell.
Can't we do this by having a map which is updated every time the focus changes?
So, this means that on blur of a cell we update the Sheet and recalc
So, can we have a behaviour which has all the 'moves' of each cell (ie. UI.unions?)
and on any move, saves the focused cell, recalcs, rewrites all cells and then moves to the next?
The move would then mean - set string back to the input one, make focus.

-}

setup :: IORef (Sheet String)-> Window -> UI ()
setup rSheet window = void $ do   
    sheet <- liftIO $ readIORef rSheet
    let (ncols, nrows) = lastCell sheet
    
    set title "Spreadsheet" $ return window     
    -- make vanilla input cells with no behaviour
    cellss <- sequence $ map (\row-> sequence $ map (\col-> 
                                                        simpleCell rSheet $ fromCoords (col, row) 
                                                    ) [1..ncols]
                             ) [1..nrows]

    -- This behaviour has the value of each cell
    bInputss <- uiApply cellss (\(cell,ref) -> do  
                                                let e = fmap (\a-> (a, ref)) $ UI.valueChange cell
                                                stepper ("", fromCoords (1,1)) e
                               )

    -- These behaviours have the value cells on blurring
    -- If we set cells to blur on move these should be triggered?
    -- We need the ref as well, otherwise we don't know where in the Sheet to save the value
    let blurValss = concatMap (\(cs, bInputs) -> fmap (\(cell, bInput) -> bInput <@ UI.blur cell) $ zip cs bInputs ) $ zip cellss bInputss

    -- So, this behaviour has the value of the LATEST blurred cell and it's ref
    -- bBlur :: Behavior (String, Ref)
    bBlur <- stepper ("", fromCoords (1,1)) $ fmap head $ UI.unions blurValss        
        
    -- What to do when the cell is blurred
    --      Work out the new input sheet
    --      Save a new sheet to the IORef
    --      Work out the new output sheet
    --      Write new output to all cells
    onChanges bBlur (\(str, ref) -> do
                        liftIO $ updateSheet (str, ref) rSheet
                        newInput <- liftIO $ readIORef rSheet
                        let output = recalcSheet $ parseSheet newInput
                        uiApply cellss (\(cell, ref) -> writeCell cell $ output!ref)
                    )
        
    -- After all this we need to get the cells to display the user input when on focus
    finalCells <- uiApply cellss (\(cell, ref) -> addFocusAndMoveBehaviour rSheet ref cell)

    let colnames = (C.string ""):[C.string $ [chr $ ord 'A' + c - 1]|c<-[1..ncols]]
        rows = zipWith (\r rowCells -> (C.string $ show r) : map element rowCells) [1..nrows] cellss

    -- This is an input field for the file name
    nameInput <- UI.input
    let displayNameInput = do
            ss <- liftIO $ readIORef rSheet
            element nameInput # set UI.value (name ss)

    -- Buttons to save and load files
    saveButton <- UI.button # set UI.text "Save"
    loadButton <- UI.button # set UI.text "Load"
    on UI.click saveButton $ \_ -> do 
                                    fileName <- get UI.value nameInput
                                    sheet <- liftIO $ readIORef rSheet
                                    liftIO $ writeFile fileName $ showSheet sheet
    on UI.click loadButton $ \_ -> do 
                                    fileName <- get UI.value nameInput
                                    sheet <- liftIO $ readSheet fileName
                                    liftIO $ writeIORef rSheet sheet
    -- Add all the elements to the window
    getBody window #+ [ grid [[grid $ colnames:rows]]] #+ [displayNameInput] #+ [element saveButton] #+ [element loadButton]

-- | Apply takes a grid of elements and a function from these elements and the ref of each
--   and it applies the function to each element and returns the new ones
uiApply :: [[Element]] -> ((Element, Ref) -> UI a) -> UI [[a]]
uiApply cellss f = sequence $ map (\(cs, row) -> 
                                    sequence $ map (\(cell, col) -> do  
                                                        f (cell, fromCoords (col, row))
                                                   ) $ zip cs [1..]
                               ) $ zip cellss [1..]

-- | Evaluate and print the cellFns here is the LOEB!
recalcSheet :: Sheet CellFn -> Sheet String
recalcSheet fs = fmap (show.runId.eval) $ loeb fs

-- | Parse the user input stings to the CellFns in each cell
parseSheet :: Sheet String -> Sheet CellFn
parseSheet = fmap ((either (const $ sval "Parse error") id).(parse expr ""))

-- | We can map over this Behavior and save the entry into the IORef sheet
updateSheet :: (String, Ref)->IORef (Sheet String) -> IO ()
updateSheet (str, ref) rSheet = do
    oldSheet <- liftIO $ readIORef rSheet
    let newSheet = Sheet (name oldSheet) (focus oldSheet) $ (cells oldSheet)//[(ref,str)] 
    liftIO $ writeIORef rSheet newSheet 
        
writeCell :: Element -> String -> UI Element
writeCell cell val = do return cell # set UI.value val
        
simpleCell :: IORef (Sheet String) -> Ref -> UI Element
simpleCell rSheet ref =  do
    sheet <- liftIO $ readIORef rSheet
    let (ncols, nrows) = lastCell sheet
        (col, row) = toCoords ref
        tabIndex = col * nrows + row
    UI.input # set (attr "tabindex") (show tabIndex) # set UI.id_ (show ref) 

addFocusAndMoveBehaviour :: IORef (Sheet String) -> Ref -> Element -> UI Element
addFocusAndMoveBehaviour rSheet ref input = do
    -- Set the on focus behaviour so that it save the user input value
    sheet <- liftIO $ readIORef rSheet
    bUserInput <- stepper ((sheet)!ref) $ UI.valueChange input
    bValue <- stepper ((sheet)!ref) $ bUserInput <@ UI.focus input
    sink UI.value bValue $ return input
    -- Set the move behaviour - move when move key is pressed
    let bs = snd $ bounds $ cells $ sheet
        move :: Ref -> UI ()
        move direction = do
            setBlur input
            next <- getCell $ refAdd bs ref direction
            UI.setFocus next
            
    on UI.keydown input $ \c ->case c of
                                37 -> do move rLeft
                                38 -> do move rUp
                                39 -> do move rRight
                                40 -> do move rDown
                                13 -> do move rDown
                                _ -> return ()   
    return input

-- This just makes the cell - then we can call 
makeCell :: IORef (Sheet String) -> Ref -> UI Element
makeCell rSheet refIn = do
    sheet <- liftIO $ readIORef rSheet
    let (ncols, nrows) = lastCell sheet
    let (x,y)= toCoords refIn
    let tabIndex = x * nrows + y

    UI.input # set (attr "tabindex") (show tabIndex)  
             # set UI.id_ (show refIn) 

setCell :: Element -> String -> UI ()
setCell e s = do 
    set UI.value s (return e)
    return ()


-- Sets the behaviour of the cell
renderMove :: IORef (Sheet String) -> Ref -> [[Element]] -> Element -> UI Element
renderMove rSheet refIn eCellss input = do
    sheet <- liftIO $ readIORef rSheet
    let bs = lastRef sheet

    on UI.focus input (\_ -> do
                            ss <- liftIO $ readIORef rSheet
                            (return input) # set UI.value ((ss)!refIn)
                            return ()
                      )

    let recalc :: UI ()
        recalc = do
            val <- get UI.value input
            let newsSheet =  Sheet (name sheet) (focus sheet) $ (cells sheet)//[(refIn,val)] -- sheet of input strings
            liftIO $ writeIORef rSheet newsSheet
            ss <- liftIO $ readIORef rSheet
            --let vs = fmap (show.runId.eval)$ fs =>> wfix -- sheet of value strings
            let vs = recalcSheet $ parseSheet ss -- sheet of Value strings
            sequence $ map (\(eCells, col) -> sequence $ map (\(eCell,row) -> setCell eCell $ (vs)!(fromCoords (row, col))) $ zip eCells [1..]) $ zip eCellss [1..]
            return ()

        -- | Saves input string and parsed CellFn
        -- sets cell value to value string
        -- blurs current cell
        -- finds next cell
        -- sets focus of next cell
        move :: Ref -> UI ()
        move direction = do
            recalc
            setBlur input
            next <- getCell $ refAdd bs refIn direction
            UI.setFocus next
            
    let moveAway :: Element -> Event Int
        moveAway = (\e -> let k = UI.keydown e
                              m = fmap (const (-1)) $ UI.leave e
                          in unionWith (\k m -> k) k m
                                    
                   )
    on moveAway input $ \c ->case c of
                        37 -> do move rLeft
                        38 -> do move rUp
                        39 -> do move rRight
                        40 -> do move rDown
                        13 -> do move rDown
                        _  -> do move rZero
                                
    return input

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

-- Sets a cell to blurred - copied from UI.setFocus
setBlur :: Element -> UI ()
setBlur = runFunction . ffi "$(%1).blur()"

getCell :: Ref -> UI Element
getCell r =  do
    w <- askWindow    
    e <- getElementById w $ show r
    return $ fromJust e


{-******************************************************************************************************

CODE GRAVEYARD

******************************************************************************************************-}

setupOld :: IORef (Sheet String)-> Window -> UI Element
setupOld rSheet window = do   
    sheet <- liftIO $ readIORef rSheet
    let (ncols, nrows) = lastCell sheet

    set title "Spreadsheet" $ return window     
    -- make the cells and set the in-cell behaviour
    cellFieldss1 <- sequence $ map (\row-> 
            sequence $ map (\col-> do
                let ref = Ref (CAbs col) (RAbs row)
                makeCell rSheet ref -- $ show $ runId $ eval $ (cells (sheet =>> wfix))!ref
            ) [1..ncols]
        ) [1..nrows]

    cellFieldss2 <- sequence $ map (\(cells, row) -> 
            sequence $ map (\(cell, col)-> 
                renderMove rSheet (Ref (CAbs col) (RAbs row)) cellFieldss1 cell
            ) $ zip cells [1..ncols]
        ) $ zip cellFieldss1 [1..nrows]

    let colnames = (C.string ""):[C.string $ [chr $ ord 'A' + c - 1]|c<-[1..ncols]]
        rows = zipWith (\r rowCells -> (C.string $ show r) : map element rowCells) [1..nrows] cellFieldss1

    sheetInput <- UI.span
    let displaySheetInput :: UI Element
        displaySheetInput = do
            ss <- liftIO $ readIORef rSheet
            element sheetInput # set text (show ss)
            return sheetInput

    saveButton <- UI.button # set UI.text "Save"

    displaySheetInput   
    
    on UI.click saveButton $ \_ -> return ()
    getBody window #+ [ grid [[grid $ colnames:rows]]] #+ [displaySheetInput, displaySheetInput, element saveButton]


{-

    --bMove <- stepper 0 $ filterE (\k-> (k==37)||(k==38)||(k==39)||(k==40)||(k==40)) $ UI.keydown input    
    --onChanges bMove ( \_-> recalc )   

    --bUserInput <- stepper ((cells sSheet)!refIn) $ UI.valueChange input
    --let bParsedInput = (parseToValue fSheet refIn) <$> bUserInput
    -- This determines what is shown in a cell
    --bValue <- stepper "" $ fmap head $ UI.unions
        --[ bParsedInput <@ UI.blur input  -- calculate the value when we leave the cell
        --, bUserInput <@ UI.focus input   -- return to user input when go back
        --]
    --sink UI.value bValue $ return input




oldRenderCell :: IORef (Sheet CellFn) -> Ref -> UI Element
oldRenderCell rSheet refIn = do
    let (x,y)= toCoords refIn
    let tabIndex = x * nrows + y

    input  <- UI.input # set (attr "tabindex") (show tabIndex)  # set UI.id_ (show refIn)

    sheet <- liftIO $ readIORef rSheet
    let bs = snd $ bounds $ cells $ sheet

        move :: Ref -> UI ()
        move direction = do
            setBlur input
            next <- getCell $ refAdd bs refIn direction
            UI.setFocus next

    on UI.keydown input $ \c ->case c of
                                37 -> do move rLeft
                                38 -> do move rUp
                                39 -> do move rRight
                                40 -> do move rDown
                                13 -> do move rDown
                                _ -> return ()

    bUserInput <- stepper "" $ UI.valueChange input

    let bParsedInput = (parseToValue sheet refIn) <$> bUserInput
      
    -- Try the on version first...
    on UI.valueChange input $ const $ void $ do  
        val <- get UI.value input
        let newSheet = parseToSheet sheet refIn val
        liftIO $ writeIORef rSheet newSheet
              
    bValue <- stepper "" $ fmap head $ UI.unions
        [ bParsedInput <@ UI.blur input  -- calculate the value when we leave the cell
        , bUserInput <@ UI.focus input   -- return to user input when go back
        ]

    sink UI.value bValue $ return input

-}

