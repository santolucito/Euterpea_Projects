import FRP.Helm
import qualified FRP.Helm.Keyboard as Keyboard
import qualified FRP.Helm.Window as Window

data State = State { mx :: Double, my :: Double }

step :: (Int, Int) -> State -> State
step (dx, dy) state = state { mx = (10 * (realToFrac dx)) + mx state,
                              my = (10 * (realToFrac dy)) + my state }

render :: (Int, Int) -> State -> Element
render (w, h) (State { mx = mx, my = my }) =
  centeredCollage w h [move (mx, my) $ filled white $ square 100]

main :: IO ()
main = run defaultConfig $ render <~ Window.dimensions ~~ stepper
  where
    state = State { mx = 0, my = 0 }
    stepper = foldp step state Keyboard.arrows
