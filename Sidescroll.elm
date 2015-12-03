import Color exposing (..)
import Debug
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (..)
import Window

type alias Model = 
    { x : Float }

model : Model
model = {
    x = -400.0
    }

view m =
    Debug.watch (toString m)
    collage 800 600
        [
            rect 800 600
                |> filled (rgb 174 228 312)
        ,   circle 20.0 |> filled (rgb 20 70 127) |> move (m.x, 0)
        ]

update : Float -> Model -> Model
update f m =
    let new = if (m.x >= 400.0) then -400 else m.x + 3
    in { x = new }

main =
    Signal.map view (Signal.foldp update model (fps 30))

