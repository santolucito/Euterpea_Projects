
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
                             energy :: Double,
			     npc :: Bool}
			     --vx :: Double,
			     --vy :: Double,
			    -- dir :: String }

bird = Character 300 750 False
obstacle1 = Character 800 500 True
obstacle2 = Character 1000 200 True


-- UPDATE -- ("m" is for Mario)

step :: Bool -> [Character] -> [Character]
step keys cs = map (update keys) cs

update keys c | npc c == True = (npcUpdate c) True 
              | npc c == False = (playerUpdate keys c) False

npcUpdate :: Character -> Bool -> Character
npcUpdate c
    | x c < 0 = Character (800) (energy c)
    | x c >= 0 = Character ((x c)-1) (energy c)

playerUpdate :: Bool -> Character -> Bool -> Character
playerUpdate keys c
    | energy c > 751 = Character ((x c)+0.5) (750)
    | energy c < 0 = Character ((x c)+0.5) (0)
    | keys == True = Character ((x c)-1) ((energy c)-3)
    | keys == False = Character ((x c)+0.5) ((energy c)+1.5)


-- DISPLAY

--need to scale to windows dimensions
render :: [Character] -> (Int,Int) -> Element
render cs (w,h) = collage w h $ concat (map my_collage cs)

my_collage :: Character -> [Form]
my_collage c
      | npc c == False = (healthBar c) ++ (player c)
      | npc c == True = obs c

healthBar c = [rect ((*2) $ energy c) 25 |> filled green]
player c = [move (400, x c) $ filled white $ square 50]
obs c = [move (x c , energy c) $ filled red $ rect 25 100]

-- INPUT
runAt c s = let x = lift2 (,) c s
            in lift snd x

input = runAt (Time.fps 60) (Keys.isDown Keys.SpaceKey) 


{-| Bootstrap the game. -}
main :: IO ()
main = do
    run config $ render <~ stepper ~~ Window.dimensions
  
  where
    config = defaultConfig { windowTitle = "Helm - Flappy" }
    stepper = foldp step [bird,obstacle1,obstacle2] input
