{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}

module Cell ( Cell(..), CellFn(..), emptySheet, emptySheetF, printCalcedSheet, zeroSheet, nval,  bval, sval, eadd, emul, ediv, esub, epow, eand, eor, exor, egt, elt, eeq, econcat, enfunc, ebfunc, esfunc, eref) where

import Text.Printf
import Control.Monad
import Data.Array

import Cat
import Refs
import Value
import Expr
import Str


-- | This file describes the cell type.
-- It also has a lot of functions to help the parser to create
-- the Cell when it has parsed the string that has been entered in the cell.

-- A cell must contain one of the 3 Expr types or be empty
data Cell e = CA (Arithmetic e) | CL (Logic e) | CS (Str e)  | CR Ref (Sheet e) | CE


-- A show function that leaves out all the type stuff
instance Show e => Show (Cell e) where
    show (CA x) = show x
    show (CL x) = show x
    show (CS x) = show x
    show (CR r s) = show $ (cells s)!r
    show CE = ""

-- Just apply f to the contents of the Cell
instance Functor Cell where
	fmap  f (CA x)  = CA $ fmap f x
	fmap  f (CL x)  = CL $ fmap f x
	fmap  f (CS x)  = CS $ fmap f x
	fmap  f (CR r s)  = CR r $ fmap f s
	fmap  f CE  = CE

-- Similarly for the Eval instance
instance Monad m => Eval Cell m where
    evalAlg :: Cell (m Value) -> m Value
    evalAlg (CA x) = evalAlg x
    evalAlg (CL x) = evalAlg x
    evalAlg (CS x) = evalAlg x
    evalAlg (CE) = evalAlg (CS $ SVal "")

type CellFn = Sheet (Fix Cell) -> Fix Cell

-- This doesn't work because CellFn is a function from a sheet to a cell
-- We could save the input string here as well
instance Show CellFn where
	show :: CellFn -> String
	show f = "Function to: " ++ (show $  (f $ zeroSheet 10 10))

-- Create a sheet full of zeros - for when we load a new spreadsheet
-- no good any more now sheets have functions in each cell?
zeroSheet :: Int -> Int -> Sheet (Fix Cell)
zeroSheet n m = Sheet "Zero" (fromCoords (0,0)) $ listArray (fromCoords (0,0), fromCoords ((n-1),(m-1))) $ all
    	where
            all :: [Fix Cell]
            all = take (m*n) $ repeat $ inject $ CA $ AVal 0.0

-- Old style sheet with standalone cells
sheets :: [Sheet (Fix Cell)]
sheets = [zeroSheet 10 10]

-- Sheet with references - so each cell is a function from a sheet of Fix Cells to a Fix Cell
emptySheetF :: Sheet CellFn
emptySheetF = Sheet "Empty" (fromCoords (0,0)) $ listArray (fromCoords (0,0), fromCoords (-1,-1)) []

-- Create an initial sheet - presentlry 
emptySheet :: Int -> Int -> Sheet CellFn
emptySheet m n = Sheet "Empty" (fromCoords (0,0)) $ listArray (fromCoords (0,0), fromCoords (m-1,n-1)) $ all
    	where
            all :: [CellFn]
            all = take (m*n) $ fmap (\i-> nval $ fromIntegral i) [1..]

printCalcedSheet :: Sheet (Fix Cell) -> IO ()
printCalcedSheet ss = 
	forM_ [ymin..ymax] $ \i -> do
		forM_ [xmin..xmax] $ \j ->
			printf "%s   " (show $ (cells wss) ! (fromCoords (j,i)))
		printf "\n"
	where
		xmin = x $ cRef $ fst $ bounds $ cells ss
		xmax = x $ cRef $ snd $ bounds $ cells ss
		ymin = y $ rRef $ fst $ bounds $ cells ss
		ymax = y $ rRef $ snd $ bounds $ cells ss
		wss = fmap (runId.eval) ss	
		cout :: Sheet String
		cout = fmap (\c -> printf "%s\n" $ show c) ss	
		xss = fmap (show.runId.eval) $ cells ss		


-- These are helper functions to do all the injecting etc for fixed
-- types. The parser will use these as output when it's decided what
-- the cell is.

instance Sheet :<: Cell where
    inj :: Sheet a -> Cell a
    inj s = CA $ NRef s (focus s)

-- Basic value types
nval :: Double -> CellFn
nval n = finject $ CA $ AVal n
bval :: Bool -> CellFn
bval n = finject $ CL $ LVal n
sval :: String -> CellFn
sval n = finject $ CS $ SVal n
-- Empty cell
noval :: CellFn
noval = finject $ CE

-- Arithmetic expression functions
eadd :: (Cell :<: f) => (Sheet (Fix f) -> Fix f) -> (Sheet (Fix f) -> Fix f) -> (Sheet (Fix f) -> Fix f)
eadd x y = \ss -> inject $ CA $ Add (x ss) (y ss)

emul :: (Cell :<: f) => (Sheet (Fix f) -> Fix f) -> (Sheet (Fix f) -> Fix f) -> (Sheet (Fix f) -> Fix f)
emul x y = \ss -> inject $ CA $ Mul (x ss) (y ss)

ediv :: (Cell :<: f) => (Sheet (Fix f) -> Fix f) -> (Sheet (Fix f) -> Fix f) -> (Sheet (Fix f) -> Fix f)
ediv x y = \ss -> inject $ CA $ Div (x ss) (y ss)

esub :: (Cell :<: f) => (Sheet (Fix f) -> Fix f) -> (Sheet (Fix f) -> Fix f) -> (Sheet (Fix f) -> Fix f)
esub x y = \ss -> inject $ CA $ Sub (x ss) (y ss)

epow :: (Cell :<: f) => (Sheet (Fix f) -> Fix f) -> (Sheet (Fix f) -> Fix f) -> (Sheet (Fix f) -> Fix f)
epow x y = \ss -> inject $ CA $ Pow (x ss) (y ss)

-- Boolean expression functions
eand :: (Cell :<: f) => (Sheet (Fix f) -> Fix f) -> (Sheet (Fix f) -> Fix f) -> (Sheet (Fix f) -> Fix f)
eand x y = \ss -> inject $ CL $ And (x ss) (y ss)

eor :: (Cell :<: f) => (Sheet (Fix f) -> Fix f) -> (Sheet (Fix f) -> Fix f) -> (Sheet (Fix f) -> Fix f)
eor x y = \ss -> inject $ CL $ Or (x ss) (y ss)

exor :: (Cell :<: f) => (Sheet (Fix f) -> Fix f) -> (Sheet (Fix f) -> Fix f) -> (Sheet (Fix f) -> Fix f)
exor x y = \ss -> inject $ CL $ Xor (x ss) (y ss)

egt :: (Cell :<: f) => (Sheet (Fix f) -> Fix f) -> (Sheet (Fix f) -> Fix f) -> (Sheet (Fix f) -> Fix f)
egt x y = \ss -> inject $ CL $ LGT (x ss) (y ss)

elt :: (Cell :<: f) => (Sheet (Fix f) -> Fix f) -> (Sheet (Fix f) -> Fix f) -> (Sheet (Fix f) -> Fix f)
elt x y = \ss -> inject $ CL $ LLT (x ss) (y ss)

eeq :: (Cell :<: f) => (Sheet (Fix f) -> Fix f) -> (Sheet (Fix f) -> Fix f) -> (Sheet (Fix f) -> Fix f)
eeq x y = \ss -> inject $ CL $ LEQ (x ss) (y ss)

-- String exprtession functions
econcat :: (Cell :<: f) => (Sheet (Fix f) -> Fix f) -> (Sheet (Fix f) -> Fix f) -> (Sheet (Fix f) -> Fix f)
econcat x y = \ss -> inject $ CS $ Concat (x ss) (y ss)

-- The spreadsheet built in functions for each value type
--enfunc :: (Cell :<: f) => String -> [Fix f]-> Fix f
enfunc s ps = \ss -> inject $ CA $ NFunc s $ fmap (\p -> p ss) ps

--ebfunc :: (Cell :<: f) => String -> [Fix f]-> Fix f
ebfunc s ps = \ss -> inject $ CL $ BFunc s $ fmap (\p -> p ss) ps

--esfunc :: (Cell :<: f) => String -> [CellFn]-> CellFn
esfunc s ps = \ss -> inject $ CS $ SFunc s $ fmap (\p -> p ss) ps

-- References
eref :: Ref -> CellFn
eref r = \ss-> (cells ss)!r






