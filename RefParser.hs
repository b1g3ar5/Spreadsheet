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

module RefParser (parseRefsSheet, refexpr) where

import System.Environment
import Text.ParserCombinators.Parsec -- hiding (string)
import Control.Applicative ((<*), (<$>), (<*>))
import Control.Monad (liftM)
import Data.Array
import Data.Char
import Data.Map as M hiding (map, foldl)
import Data.Maybe
import Data.Either (rights)
import Data.Either.Utils (fromRight)
import Data.List
import Data.Monoid
import Data.Function

import Sheet 
import Expr
import Cell
import Ref
import Cat


-- | Parse the user input stings to the CellFns in each cell
parseRefsSheet :: Sheet String -> Sheet CellFn
parseRefsSheet = fmap ((either (const $ sval "Parse error") id).(parse refexpr ""))

refexpr :: Parser CellFn
refexpr = do (try fexpr) <|> pstring

-- | This is the exported parser. It parses one of a numerical, string or boolean expression
fexpr :: Parser CellFn
fexpr = do char '='
           (try bexpr) <|> (try sexpr) <|> (nexpr)

-- | Parse a numerical expression in parentheses
parenNexpr:: Parser CellFn
parenNexpr = do char '('
                whitespace
                e <- nexpr
                char ')'
                return e

-- | Parse a string expression in parentheses
parenSexpr:: Parser CellFn
parenSexpr = do char '('
                whitespace
                e <- sexpr
                char ')'
                whitespace
                return e

-- | Parse a boolean expression in parentheses
parenBexpr:: Parser CellFn
parenBexpr = do char '('
                whitespace
                e <- bexpr
                char ')'
                whitespace
                return e

-- | Parse a numerical expression
nexpr :: Parser CellFn
nexpr = do
          first <- mulTerm
          ops <- addOps
          return $ foldl buildExpr first ops
       where buildExpr acc ('+', x) = x ++ acc
             buildExpr acc ('-', x) = x ++ acc

-- | Parse a string expression
sexpr:: Parser CellFn
sexpr = do first <- sterm
           ops <- concatOps
           return $ foldl buildExpr first ops
           where buildExpr acc ('&', x) = x ++ acc

-- | Parse a boolean expression
bexpr :: Parser CellFn
bexpr = do
          first <- andTerm
          ops <- orOps
          return $ foldl buildExpr first ops
       where buildExpr acc ('|', x) = x ++ acc

-- | Second level of numerical terms - after addition, before powers
mulTerm:: Parser CellFn
mulTerm = do first <- powTerm
             ops <- mulOps
             return $ foldl buildExpr first ops
          where buildExpr acc ('*', x) = x ++ acc
                buildExpr acc ('/', x) = x ++ acc

-- | Third level of numerical terms - after multiplication
powTerm:: Parser CellFn
powTerm = do first <- nterm
             ops <- powOps
             return $ foldl buildExpr first ops
          where buildExpr acc ('^', x) = x ++ acc

-- | Parse a built in numerical function, or a numerical terminal
nfuncTerm :: Parser CellFn
nfuncTerm = (try nfunc) <|> nterm


-- | Parse the boolean AND terms
andTerm:: Parser CellFn
andTerm = do first <- compTerm 
             ops <- andOps
             return $ foldl buildExpr first ops
          where buildExpr acc ('&', x) = x ++ acc

-- This is a comparison term eg 1>3, True=False, "Nick">"Straw"
compTerm :: Parser CellFn
compTerm = do (try bcomp) <|> (try scomp) <|> (try ncomp) <|> (try bterm)

-- Numerical comparisons
ncomp :: Parser CellFn
ncomp = do x <- nterm;
		   op <- compOp;
		   y <- nterm;
		   return $ x ++ y

-- Boolean comparisons
bcomp :: Parser CellFn
bcomp = do x <- bterm;
		   op <- compOp;
		   y <- bterm;
		   return $ x ++ y

-- String comparisons
scomp :: Parser CellFn
scomp = do x <- sterm;
		   op <- compOp;
		   y <- sterm;
		   return $ x ++ y

addOps :: Parser [(Char, CellFn)]
addOps = many addOp

mulOps :: Parser [(Char, CellFn)]
mulOps = many mulOp

powOps :: Parser [(Char, CellFn)]
powOps = many powOp

orOps :: Parser [(Char, CellFn)]
orOps = many orOp

andOps :: Parser [(Char, CellFn)]
andOps = many andOp

concatOps :: Parser [(Char, CellFn)]
concatOps = many concatOp

addOp :: Parser (Char, CellFn)
addOp = do op <- oneOf "+-"
           whitespace
           t <- mulTerm 
           whitespace
           return (op, t)

compOp :: Parser Char
compOp = do op <- oneOf ">=<"; 
            whitespace
            return op

mulOp :: Parser (Char, CellFn)
mulOp = do op <- oneOf "*/"
           whitespace
           t <- powTerm
           whitespace
           return (op, t)

powOp :: Parser (Char, CellFn)
powOp = do op <- oneOf "^"
           whitespace
           t <- nterm
           whitespace
           return (op, t)

concatOp :: Parser (Char, CellFn)
concatOp = do op <- oneOf "&"
              whitespace
              t <- sterm
              whitespace
              return (op, t)

andOp :: Parser (Char, CellFn)
andOp = do op <- string "&&"
           whitespace
           t <- compTerm -- bterm
           whitespace
           return (head op, t)

orOp :: Parser (Char, CellFn)
orOp = do op <- string "||"
          whitespace
          t <- andTerm -- bterm
          whitespace
          return (head op, t)

nterm :: Parser CellFn
nterm = do t <- term'
           whitespace
           return t
        where term' = (try $ number) <|> (try parenNexpr) <|> (try $ nfunc) <|> (try nRef)

sterm :: Parser CellFn
sterm = do t <- sterm'
           whitespace
           return t
        where sterm' = (try $ qstring) <|> (try parenSexpr) <|> (try $ sfunc) <|> (try sRef)

bterm :: Parser CellFn
bterm = do t <- bterm'
           whitespace
           return t
        where bterm' = (try $ pbool) <|> (try parenBexpr) <|> (try $ bfunc)  <|> (try bRef)


-- Quoted string
qstring :: Parser CellFn
qstring = do
     char '"'
     s <- manyTill (noneOf ("\"")) (char '"')
     whitespace
     return $ []

pstring :: Parser CellFn
pstring = do
    char '"'
    s <- manyTill (noneOf ("\"")) (char '"')
    whitespace
    return $ []

s2b::String->Bool
s2b s = if (map toUpper s)=="TRUE" then True else False

pbool :: Parser  [Ref]
pbool = do
	 whitespace
	 s <- string "True" <|> string "False";
	 return $ []

pDigit:: Parser Char
pDigit = oneOf ['0'..'9']
pSign:: Parser Char
pSign = option '+' $ oneOf "-+"
pDigits:: Parser [Char]
pDigits = many1 pDigit
pDecimalPoint :: Parser Char
pDecimalPoint = char '.'
pFracPart :: Parser [Char]
pFracPart = option "0" (pDecimalPoint >> pDigits)

number :: Parser CellFn
number = do sign <- pSign
            integerPart <- pDigits
            fracPart <- pFracPart
            expPart <- pExp
            let i = read integerPart
            let f = read fracPart
            let e = expPart
            let value = (i + (f / 10^(length fracPart))) * 10 ^^ e
            return $ rval []
         where pExp = option 0 $ do
                             oneOf "eE"
                             sign <- pSign
                             num <- pDigits
                             let n = read num
                             return $ if sign == '-' then negate n else n


whitespace = many $ oneOf "\n\t\r\v "

nfunc :: Parser CellFn
nfunc = do
    fname <- manyTill letter (char '(')
    ps <- nexpr `sepBy` (char ',')
    char ')'
    return $ concat ps


sfunc :: Parser CellFn
sfunc = do
    char '@'
    fname <- manyTill letter (char '(')
    ps <- sexpr `sepBy` (char ',')
    char ')'
    return $ concat ps

bfunc :: Parser CellFn
bfunc = do
    char '@'
    fname <- manyTill letter (char '(')
    ps <- bexpr `sepBy` (char ',')
    char ')'
    return $ \ss -> concatMap (\f -> f ss) ps


pAbs:: Parser Char
pAbs = char '$'

pAlpha :: Parser Char
pAlpha = oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"


nRef :: Parser  [Ref]
nRef = fmap (\x->[x]) pRef

bRef :: Parser  [Ref]
bRef = fmap (\x->[x]) pRef

sRef :: Parser  [Ref]
sRef = fmap (\x->[x]) pRef

pRef :: Parser Ref
pRef = try pAbsAbs <|> try pAbsRel <|> try pRelAbs <|> pRelRel


-- So, A goes to 1, column counting starts at 1
u2i :: Char -> Int
u2i c = ord (toUpper c) - 64

pAbsAbs :: Parser Ref
pAbsAbs = do 
		ac <- pAbs
		c <- pAlpha
		rc <- pAbs
		r <- pDigits
		return $ Ref (CAbs $ u2i c) (RAbs (read r))

pAbsRel :: Parser Ref
pAbsRel = do 
		ac <- pAbs
		c <- pAlpha
		r <- pDigits
		return $ Ref (CAbs $ u2i c) (RRel (read r))

pRelAbs :: Parser Ref
pRelAbs = do 
		c <- pAlpha
		rc <- pAbs
		r <- pDigits
		return $ Ref (CRel $ u2i c) (RAbs (read r))

pRelRel :: Parser Ref
pRelRel = do 
		c <- pAlpha
		r <- pDigits
		return $ Ref (CRel $ u2i c) (RRel (read r))







