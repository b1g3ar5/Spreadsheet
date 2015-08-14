{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveFunctor #-}

module Cat ( Id(..), Fix(..), HFix(..), coreturn, cojoin, cata, hcata, (:~>), I(..), J(..), HFunctor(..), HFoldable(..), HEq(..), moeb, loeb ) where

import Data.Monoid
import Data.Fix
import Control.Comonad
--import Control.Functor.Apply

-- |This file has some category theory stuff in it which I couldn't find in a common library

newtype HFix h a = HFix { unHFix :: (h (HFix h) a) }

type f :~> g = forall a. f a -> g a

hcata :: HFunctor h => (h f :~> f) -> HFix h :~> f
hcata alg = alg . hfmap (hcata alg) . unHFix

-- |Identity functor
newtype I x = I {unI :: x} deriving (Show)
-- |Identity functor with dummy type
newtype J x y = J {unJ :: x}

class HFunctor (h :: ( * -> * ) -> * -> *) where
    hfmap :: (f :~> g) -> h f :~> h g

class HFoldable (h :: (* -> *) -> * -> *) where
    hfoldmap:: Monoid m => (forall b. f b->m) -> h f a -> m

class HEq (f :: * -> *) where
    heq :: f a -> f a -> Bool


coreturn:: Comonad w => w a -> a
coreturn = extract

cojoin:: Comonad w => w a -> w (w a)
cojoin = duplicate

data Id x = Id {runId::x} deriving (Show)

instance Functor Id where
    fmap f (Id x) = Id $ f x

instance Applicative Id where
    pure a = Id a
    Id f <*> Id x = Id (f x)
    
instance Monad Id where
    return x = Id x
    (Id x) >>= f = f x

moeb :: (((a -> b) -> b) -> c -> a) -> c -> a
moeb f x = go where go = f ($ go) x

loeb :: Functor f => f (f a -> a) -> f a
loeb = moeb fmap





