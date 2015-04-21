import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (second)


main =
  Signal.map clock (Time.every second)


clock t =
  collage 400 400
    [ filled    lightGrey   (ngon 12 110)
    , outlined (solid grey) (ngon 12 110)
    , hand orange   100  t
    , hand charcoal 100 (t/60)
    , hand charcoal 60  (t/720)
    ]


hand clr len time =
  let angle = degrees (90 - 6 * Time.inSeconds time)
  in
      segment (0,0) (fromPolar (len,angle))
        |> traced (solid clr)
