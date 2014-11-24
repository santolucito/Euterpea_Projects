
{-| Renders a pizza-like colour-wheel, with each slice being a unique pre-defined color.
    See http://helm-engine.org/guide/colors/ -}
module Main where

import FRP.Helm
import qualified FRP.Helm.Time as Time
import qualified FRP.Helm.Window as Window
import qualified FRP.Helm.Keyboard as Keys
import qualified FRP.Helm.Sample as S

-- MODEL
data Character = Character { x :: Int}
			     --y :: Int,
			     --vx :: Double,
			     --vy :: Double,
			    -- dir :: String }

mario = Character 0


-- UPDATE -- ("m" is for Mario)

step keys mario  | keys == True = Character $ (x mario)+1
                 | keys == False = Character $ (x mario)
                -- | fst keys == -1 = Character $ (x mario)-1 
                -- | fst keys == 0 = Character (x mario)


-- DISPLAY

render :: Character -> (Int,Int) -> Element
render c (w,h) = collage w h [rect (fromIntegral $ (*20) $ x c) 25 |> filled red]


{-
-- MARIO
input = let delta = lift (\t -> t/20) (fps 25)
        in  sampleOn delta (lift2 (,) delta Keyboard.arrows)
-}
--input' = runAt (Time.fps 60) Keys.arrows
runAt c s = let x = lift2 (,) c s
            in lift snd x

input = runAt (Time.fps 60) (Keys.isDown Keys.SpaceKey) 


{-| Bootstrap the game. -}
main :: IO ()
main = do
    run config $ render <~ stepper ~~ Window.dimensions
  
  where
    config = defaultConfig { windowTitle = "Helm - Mario" }
    stepper = foldp step mario input
