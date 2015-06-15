{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}

module Cell ( bval0, nval0, sval0, rval0, err0, cfor, cfadd, recalcSheet, Cell(..), CellFn(..), emptySheet, printCalcedSheet, nval,  bval, sval, rval, eadd, emul, ediv, esub, epow, eand, eor, exor, egt, elt, eeq, econcat, enfunc, ebfunc, esfunc, eref) where

import Text.Printf
import Control.Monad
import Data.Array hiding ((!))
import Data.Foldable hiding (forM_, fold, or)
import Data.Monoid

import Cat
import Ref
import Value
import Expr
import Str
import Sheet
import Refs


-- | This file describes the cell type.
--   It also has a lot of functions to help the parser to create
--   the Cell when it has parsed the string that has been entered in the cell.

-- | A cell must contain one of the 3 Expr types or be empty
data Cell e = CA (Arithmetic e) | CL (Logic e) | CS (Str e) | CR (Refs e) | CE

-- | A show function that leaves out all the type stuff
instance Show e => Show (Cell e) where
    show (CA x) = show x
    show (CL x) = show x
    show (CS x) = show x
    show (CR x) = show x
    show CE = ""

-- | Just apply f to the contents of the Cell
instance Functor Cell where
	fmap  f (CA x)  = CA $ fmap f x
	fmap  f (CL x)  = CL $ fmap f x
	fmap  f (CS x)  = CS $ fmap f x
	fmap  f (CR x)  = CR $ fmap f x
	fmap  f CE  = CE

-- | Similarly for the Eval instance
instance Monad m => Eval Cell m where
    evalAlg :: Cell (m Value) -> m Value
    evalAlg (CA x) = evalAlg x
    evalAlg (CL x) = evalAlg x
    evalAlg (CS x) = evalAlg x
    evalAlg (CR x) = evalAlg x
    evalAlg (CE) = evalAlg (CS $ SVal "")


unEvalAlg :: (Monad m) => Value -> Cell (m Value)
unEvalAlg (N x) = CA $ AVal x
unEvalAlg (B x) = CL $ LVal x
unEvalAlg (S x) = CS $ SVal x
unEvalAlg (R x) = CR $ Refs x
unEvalAlg (E x) = CE

--csum :: (Monad m ) => [Cell (m Value)] -> Cell (m Value)
--csum [] = CA $ AVal 0.0
--csum (a:[]) = a
--csum (a:as) = CA $ Add (evalAlg a) (evalAlg $ csum as)

--cor :: (Monad m ) => [Cell (m Value)] -> Cell (m Value)
--cor [] = CL $ LVal False
--cor (a:[]) = a
--cor (a:as) = CL $ Or (evalAlg a) (evalAlg $ csum as)

-- | For spreadsheets each cell must have a function in it from the sheet to a Fix Cell
--   then we can use moeb, loeb and the comonad stuff - wfix and cfix
type CellFn = Sheet (Fix Cell) -> Fix Cell

-- | Ors 2 CellFns
-- This uses the or function for the Value type which only works for booleans
-- So, it returns the Fix Cell for the bool or an Error
cfor :: CellFn -> CellFn -> CellFn
cfor x y = \ss -> fixup $ vor (runId $ eval $ x ss) (runId $ eval $ y ss)
    where
        fixup (B x) = inject $ CL $ LVal x
        fixup (_) = inject $ CE

-- | Adds 2 CellFns
-- This uses the add function for the Value type which only works for numbers
-- So, it returns the Fix Cell for the number or an Error
cfadd :: CellFn -> CellFn -> CellFn
cfadd x y = \ss -> fixup $ vadd (runId $ eval $ x ss) (runId $ eval $ y ss)
    where
        fixup (N z) = nval0 z
        fixup (_) = err0

fcadd :: Fix Cell -> Fix Cell -> Fix Cell
fcadd c1 c2 = fixup $ vadd (runId $ eval c1) (runId $ eval c2)
    where
        fixup (N z) = nval0 z
        fixup (_) = err0

-- | Evaluate and print the cellFns here is the LOEB!
recalcSheet :: Sheet CellFn -> Sheet String
recalcSheet fs = fmap (show.runId.eval) $ loeb fs

-- | We need to get the refs from a CellFn before the loeb because loeb calcs the 
-- references. I wonder if we could get a function similar to loeb to calculate
-- the circular references?


-- | Create an initial sheet - with numbers in it 
emptySheet :: Int -> Int -> Sheet CellFn
emptySheet m n = Sheet "Empty" (fromCoords (0,0)) $ listArray (fromCoords (0,0), fromCoords (m-1,n-1)) $ all
    	where
            all :: [CellFn]
            all = take (m*n) $ fmap (\i-> nval $ fromIntegral i) [1..]

-- | Print a sheet of Fix Cells - that is a recalculated sheet
printCalcedSheet :: Sheet (Fix Cell) -> IO ()
printCalcedSheet ss = 
	forM_ [ymin..ymax] $ \i -> do
		forM_ [xmin..xmax] $ \j ->
			--printf "%s   " (show $ (cells wss) ! (fromCoords (j,i)))
			printf "%s   " (show $ (wss) ! (fromCoords (j,i)))
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


-- | We can inject from a Sheet into a Cell
--   by creating a cell with a reference to the focussed cell of the Sheet
--   PROBLEM: How do we know that the cell is an Aritmetic/CA type?
instance Sheet :<: Cell where
    inj :: Sheet a -> Cell a
    inj s = CA $ NRef s (focus s)

-- | Some helper functions to inject types into CellFns

-- | Simple constant cells
nval0 :: Double -> Fix Cell
nval0 n = inject $ CA $ AVal n
bval0 :: Bool -> Fix Cell
bval0 n = inject $ CL $ LVal n
sval0 :: String -> Fix Cell
sval0 n = inject $ CS $ SVal n
rval0 :: [Ref] -> Fix Cell
rval0 ns = inject $ CR $ Refs ns
err0 :: Fix Cell
err0 = inject CE

-- | Some helper functions to inject types into CellFns

-- | Simple constant cells
nval :: Double -> CellFn
nval n = finject $ CA $ AVal n
bval :: Bool -> CellFn
bval n = finject $ CL $ LVal n
sval :: String -> CellFn
sval n = finject $ CS $ SVal n
rval :: [Ref] -> CellFn
rval ns = finject $ CR $ Refs ns

-- | An Error cell
noval :: CellFn
noval = finject $ CE

-- | Arithmetic expression functions
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

-- | Boolean expression functions
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

-- | The spreadsheet built in functions for each value type
--   These go from a list of functions (ie. the parameters) to a function

-- | Number functions
enfunc :: (Cell :<: f) => String -> [a -> Fix f] -> (a-> Fix f)
enfunc name ps = \ss -> inject $ CA $ NFunc name $ fmap (\p -> p ss) ps

-- | Boolean functions
ebfunc :: (Cell :<: f) => String -> [a -> Fix f] -> (a-> Fix f)
ebfunc name ps = \ss -> inject $ CL $ BFunc name $ fmap (\p -> p ss) ps

-- | String functions
esfunc :: (Cell :<: f) => String -> [a -> Fix f] -> (a-> Fix f)
esfunc name ps = \ss -> inject $ CS $ SFunc name $ fmap (\p -> p ss) ps

-- | References
eref :: Ref -> CellFn
eref r = \ss-> ss!r


