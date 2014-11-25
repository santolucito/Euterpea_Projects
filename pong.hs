
{-| Renders a pizza-like colour-wheel, with each slice being a unique pre-defined color.
    See http://helm-engine.org/guide/colors/ -}
module Main where

import FRP.Helm
import qualified FRP.Helm.Time as Time
import qualified FRP.Helm.Window as Window
import qualified FRP.Helm.Keyboard as Keys
import qualified FRP.Helm.Sample as S

import Data.List
-- MODEL
data GameObject =  Character { y :: Double,
                               energy :: Double}
                 | Obstacle { x :: Double,
                              y :: Double,
                              which :: Int}


bird = Character 300 750

obs1 = Obstacle 800 350 1
obs2 = Obstacle 1200 200 3
obs3 = Obstacle 1450 200 5

allObs = [obs1,obs2,obs3]

obsPositions = cycle $ concat $ permutations [130,433,210,540,500,200,450,333,100,400]

-- UPDATE -- ("m" is for Mario)

step :: [Bool] -> [GameObject] -> [GameObject]
step keys cs = map (update keys) cs

update :: [Bool] -> GameObject -> GameObject
update keys (Character y e)
    | y > 550 = Character (550) e
    | y < 0 = Character (0) e
    | e > 751 = Character (y+1.5) (750)
    | e < 0 = Character (y+1.5) (0) 
    | keys!!0 == True = Character (y-3) (e-3)
    | keys!!1 == True = Character (y+6) (e+1.5)
    | keys!!0 == False = Character (y+1.5) (e+1.5)

update keys (Obstacle x y w) 
    | x < 0 = Obstacle (800) (obsPositions!!w) (w+1)
    | x >= 0 = Obstacle (x-(fromIntegral w/5)-5) y w

-- DISPLAY

--need to scale to windows dimensions
render :: [GameObject] -> (Int,Int) -> Element
render cs (w,h) = collage w h $ concat (map my_collage cs)

my_collage :: GameObject -> [Form]
my_collage (Character y e) = (healthBar e) ++ (player y)
my_collage (Obstacle x y w) = obs x y w

healthBar :: Double -> [Form]
healthBar e = [rect ((*2) $ e) 35 |> filled green]
player y = [move (400, y) $ toForm $ image 50 50 "player.png"]
obs x y w = [move (x,y) $ filled red $ rect 25 100]

-- INPUT
runAt c s = let x = lift2 (,) c s
            in lift snd x

getKeys= [(Keys.isDown Keys.SpaceKey), (Keys.isDown Keys.FKey)] 

input :: Signal[Bool]
input = runAt (Time.fps 60) $ combine getKeys


{-| Bootstrap the game. -}
main :: IO ()
main = do
    run config $ render <~ stepper ~~ Window.dimensions
  
  where
    config = defaultConfig { windowTitle = "Helm - Flappy" }
    stepper = foldp step ([bird]++allObs) input
