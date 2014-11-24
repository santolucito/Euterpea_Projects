
{-| Renders a pizza-like colour-wheel, with each slice being a unique pre-defined color.
    See http://helm-engine.org/guide/colors/ -}
module Main where

import FRP.Helm
import qualified FRP.Helm.Time as Time
import qualified FRP.Helm.Window as Window
import qualified FRP.Helm.Keyboard as Keys
import qualified FRP.Helm.Sample as S

-- MODEL
data Character = Character { x :: Double,
                             energy :: Double}
			     --y :: Int,
			     --vx :: Double,
			     --vy :: Double,
			    -- dir :: String }

bird = Character 0 750


-- UPDATE -- ("m" is for Mario)

step :: Bool -> Character -> Character
step keys bird  | energy bird > 751 = Character ((x bird)-1) (750)
                | energy bird < 0 = Character ((x bird)-1) (0)  
                | keys == True = Character ((x bird)+1) ((energy bird)-3)
                | keys == False = Character ((x bird)-0.5) ((energy bird)+1.5)


-- DISPLAY

--need to scale to windows dimensions
render :: Character -> (Int,Int) -> Element
render c (w,h) = collage w h [rect ((*2) $ energy c) 25 |> filled red]



-- INPUT
runAt c s = let x = lift2 (,) c s
            in lift snd x

input = runAt (Time.fps 60) (Keys.isDown Keys.SpaceKey) 


{-| Bootstrap the game. -}
main :: IO ()
main = do
    run config $ render <~ stepper ~~ Window.dimensions
  
  where
    config = defaultConfig { windowTitle = "Helm - Mario" }
    stepper = foldp step bird input
