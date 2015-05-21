{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

import Control.Monad
import Data.IORef

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Text.Printf
import Safe (readMay)

import Control.Applicative
import Data.Maybe


-- This file is for testing ideas

-- | Main entry point.
main :: IO ()
main = do
    ref <- newIORef "0"
    startGUI defaultConfig $ testIORef2 ref
    --startGUI defaultConfig $ barTab



testIORef :: IORef String -> Window -> UI ()
testIORef ref window = void $ do
    return window # set title "Test IORef"

    inCell  <- UI.input
    outCell <- UI.input

    -- When value changes write to IORef
    on  UI.valueChange inCell $ \_ -> do
        inValue <- get value inCell
        liftIO $ writeIORef ref inValue    

    -- function that reads the IORef and sets the value of an element with it
    let setValue oc = do  
            newVal <- liftIO $ readIORef ref
            element oc # set value newVal

    -- When enter is pressed update the value of the output
    on UI.keydown inCell $ \c -> if (c==13) then setValue outCell else return outCell

    getBody window #+ [ column [ grid [[string "In cell :", element inCell]
                                      ,[string "Out cell:", element outCell]
                                      ]
                                , string "Cells should update while typing."
                               ]
                      ]

valueUI :: ReadWriteAttr Element (UI String) String
valueUI = mkReadWriteAttr get set
    where
    get   el = callFunction $ ffi "$(%1).val()" el
    set vUI el = do
                    v<-vUI
                    runFunction  $ ffi "$(%1).val(%2)" el v

-- Annotated currency converter example
testIORef2 :: IORef String -> Window -> UI ()
testIORef2 ref window = void $ do
    return window # set title "Test IORef2"

    inCell  <- UI.input
    outCell <- UI.input
                     
    inValue <- stepper "0" $ UI.valueChange inCell

    let saveInValue :: Behavior (UI ())
        saveInValue = fmap (\v->liftIO $ writeIORef ref v) inValue
    
    inEnter <- stepper "0" $ fmap show $ filterE (\kc-> kc==13) $ UI.keydown inCell    
    let getValue = fmap (const $ liftIO $ readIORef ref) inEnter   
    element outCell # sink valueUI getValue

    getBody window #+ [ column [ grid [[string "In cell :", element inCell]
                                      ,[string "Out cell:"  , element outCell]
                                      ]
                                , string "Amounts update while typing."
                               ]
                      ]



barTab :: Window -> UI ()
barTab w = do
    -- active elements
    return w # set title "BarTab"

    elAdd    <- UI.button # set UI.text "Add"
    elRemove <- UI.button # set UI.text "Remove"
    elResult <- UI.span

    inputs   <- liftIO $ newIORef []
    
    -- functionality
    let
        displayTotal :: UI ()
        displayTotal = void $ do
            xs <- mapM (get value) =<< liftIO (readIORef inputs)
            element elResult # set text (showNumber . sum $ map readNumber xs)
        
        redoLayout :: UI ()
        redoLayout = void $ do
            layout <- mkLayout =<< liftIO (readIORef inputs)
            getBody w # set children [layout]
            displayTotal

        mkLayout :: [Element] -> UI Element
        mkLayout xs = column $
            [row [element elAdd, element elRemove]
            ,UI.hr]
            ++ map element xs ++
            [UI.hr
            ,row [UI.span # set text "Sum: ", element elResult]
            ]
        
        addInput :: UI ()
        addInput = do
            elInput <- UI.input # set value "0"
            on (domEvent "livechange") elInput $ \_ -> displayTotal
            liftIO $ modifyIORef inputs (elInput:)
        
        removeInput :: UI ()
        removeInput = liftIO $ modifyIORef inputs (drop 1)
    
    on UI.click elAdd    $ \_ -> addInput    >> redoLayout
    on UI.click elRemove $ \_ -> removeInput >> redoLayout
    addInput >> redoLayout


{-----------------------------------------------------------------------------
    Functionality
------------------------------------------------------------------------------}
type Number = Maybe Double

instance Num Number where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

readNumber :: String -> Number
readNumber s = listToMaybe [x | (x,"") <- reads s]    
showNumber   = maybe "--" show        
        


-- How to get an element using an attribute
setup :: Window -> UI ()
setup w = do
    return w # set title "Element Test"

    button1 <- UI.button # set UI.text "by tag"
    button2 <- UI.button # set UI.text "by class"
    button3 <- UI.button # set UI.text "by id"    # set UI.id_ "me"

    let mkString s = UI.string s # set UI.class_ "string"

    getBody w #+ [column $
        row [element button1, element button2, element button3]
        : map mkString (words "We are so different")]

    on UI.click button1 $ const $ do
        xs <- getElementsByTagName w "span"
        forM_ xs $ \x -> element x # set text "tag"

    on UI.click button2 $ const $ do
        xs <- getElementsByClassName w "string"
        forM_ xs $ \x -> element x # set text "class"

    Just button3 <- getElementById w "me"
    ref <- liftIO $ newIORef True
    on UI.click button3 $ const $ void $ do
        Just x <- getElementById w "me"
        b <- liftIO $ readIORef ref
        let s = if b then "yay" else "wow"
        element x # set UI.text s
        liftIO $ writeIORef ref (not b)
                


-- Annotated currency converter example
test :: Window -> UI ()
test window = void $ do
    return window # set title "Currency Converter"

    -- These are Elements
    dollar <- UI.input
    euro   <- UI.input
    
    -- This is a UI Element
    -- #+ appends child Elements onto the Element
    getBody window #+ [
            column [
                grid [[string "Dollar:", element dollar]
                     ,[string "Euro:"  , element euro  ]]
            , string "Amounts update while typing."
            ]]

    -- stepper takes an event and makes a behaviour with an initial value
    -- The event here is a value chane in an input cell
    euroIn   <- stepper "0" $ UI.valueChange euro
    dollarIn <- stepper "0" $ UI.valueChange dollar

    let
        rate = 0.7 :: Double
        -- withString takes a function and maps it over a functor and prints the result
        withString f = maybe "-" (printf "%.2f") . fmap f . readMay
    
        -- So, here we map the calculation over the Behaviour and print the result to give a Behaviour String        
        dollarOut = withString (/ rate) <$> euroIn
        euroOut   = withString (* rate) <$> dollarIn
    
    -- sets an atttribute of an Element to a Behaviour
    -- So, here it sets the value attribute of the euro element to the euroOut Behaviour String
    element euro   # sink value euroOut
    element dollar # sink value dollarOut


        
