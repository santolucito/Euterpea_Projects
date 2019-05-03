import Control.Monad (void)

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Graphics.UI.Threepenny.Events

import TimerControl

import Data.Time.Clock.POSIX
import Data.Time.Format
{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
main :: IO ()
main = startGUI defaultConfig setup

data Action = Clicked | MouseLeave 
       deriving Eq

setup :: Window -> UI ()
setup window = void $ do
    return window # set title "Kitchen Timer"

    timerVal <- UI.h1
    min <- UI.button
    sec <- UI.button
    start <- UI.button
    let buttons = [min, sec, start]     

    getBody window #+ [
            column [
                grid [[element timerVal]
                     ,[element min, element sec, element start]]
            ]]

    let
       clickAsPos :: Element -> Event Action
       clickAsPos = fmap (\_ -> Clicked) . UI.click

       leaveAsTag = fmap (\_ -> MouseLeave) . UI.leave

       buttonAsB x = stepper False $ (pure (==[Clicked])) <@> (unions [clickAsPos x, leaveAsTag x])

    -- need this to be true on button down and false on button up?
    minIn <- buttonAsB min
    secIn <- buttonAsB sec
    startIn <- buttonAsB start

    let
        cell :: a -> Behavior a -> UI (Behavior a)
        cell v c = stepper v (c <@ (unions [click min, click sec, click start]))

        secToTimestamp = formatTime defaultTimeLocale "%X" . posixSecondsToUTCTime . fromIntegral

        p_eq = (==)
        f_zero = 0
        f_countdown = (+)
        f_countup = (-)
        f_display = secToTimestamp
        f_incMinutes = (+60)
        f_incSeconds = (+1)
        i_dsp = "00:00"
        i_time = 0
        s_dt = pure 0 --TODO
        s_btnMin = (minIn :: Behavior Bool)
        s_btnSec = secIn
        s_btnStartStop = startIn

    (display, _) <- timer_without_beep
                      cell 
                      p_eq
                      f_zero
                      f_countdown
                      f_countup
                      f_display
                      f_incMinutes
                      f_incSeconds
                      i_dsp
                      i_time
                      s_dt
                      s_btnMin
                      s_btnSec
                      s_btnStartStop


    element timerVal # sink value display
