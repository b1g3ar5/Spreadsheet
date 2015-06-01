--{-# LANGUAGE GADTs #-}
--{-# LANGUAGE KindSignatures #-}
--{-# LANGUAGE RankNTypes #-}
--{-# LANGUAGE TypeOperators #-}
--{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE UndecidableInstances #-}
--{-# LANGUAGE ViewPatterns #-}
--{-# LANGUAGE DeriveFunctor #-}
--{-# LANGUAGE NoMonomorphismRestriction #-}
--{-# LANGUAGE FlexibleContexts #-}
--{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE InstanceSigs #-}


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

main :: IO ()
main = do   
    
    let fileName = "TestSheet.ss"
    sheet <- readSheet fileName
    putStrLn "OI!"

