> import FRP.Helm
> import qualified FRP.Helm.Window as Window

> render :: (Int, Int) -> Element
> render (w, h) = collage w h [move (100, 100) $ filled red $ square 64]

> main :: IO ()
> main = do
>  engine <- startup defaultConfig
>  run engine $ render <~ Window.dimensions engine
