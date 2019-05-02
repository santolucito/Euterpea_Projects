import Control.Monad (void)
import Data.Maybe
import Text.Printf
import Safe          (readMay)

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Currency

import Debug.Trace
import System.IO.Unsafe

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window = void $ do
    return window # set title "Currency Converter"

    dollar <- UI.input
    euro   <- UI.input
    
    getBody window #+ [
            column [
                grid [[string "Dollar:", element dollar]
                     ,[string "Euro:"  , element euro  ]]
            , string "Amounts update while typing."
            ]]


    euroIn   <- stepper "0" $ UI.valueChange euro
    dollarIn <- stepper "0" $ UI.valueChange dollar


    let
        rate = 0.7 :: Double

        rounder n f = (fromInteger $ round $ f * (10^n)) / (10.0^^n)

        euroToDollar :: String -> String
        euroToDollar euro   = show $ rounder 2 $ (/ rate) $ read euro
        dollarToEuro dollar = show $ rounder 2 $ (* rate) $ read dollar

        cell :: a -> Behavior a -> UI (Behavior a)
        cell v c = stepper v (c <@ (unionWith (\x y -> x) (UI.valueChange euro) (UI.valueChange dollar)))

    (dollarDisplay, euroDisplay) <- currency cell dollarToEuro euroToDollar "0" "0" dollarIn euroIn

    element dollar # sink value dollarDisplay
    element euro # sink value euroDisplay

