import Color exposing (..)
import Debug
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (..)
import Window


-- MODEL

type alias Model =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , dir : Direction
    }

type alias Burger =
    { x : Float
    , y : Float
    }

type alias Bg =
    { x : Float
    , y : Float
    }

type alias GameState =
    { mario : Model
    , burger : Burger
    , score : Int }

type Direction = Left | Right

type alias Keys = { x:Int, y:Int }

type Collision = BurgerCollision | ZombieCollision


mario : Model
mario =
    { x = 0
    , y = 0
    , vx = 0
    , vy = 0
    , dir = Right
    }

burger : Burger
burger =
    { x = 200
    , y = 0
    }

bg : Bg
bg =
    { x = 0
    , y = 0
    }

zombie : Model
zombie =
    { x = 500
    , y = 0
    , vx = 0
    , vy = 0
    , dir = Left
    }

gameState : GameState
gameState =
    { mario = mario
    , burger = burger
    , score = 0 }

-- UPDATE

update : (Float, Keys) -> GameState -> GameState
update (dt, keys) gameState =
    let mario =
      gameState.mario
          |> gravity dt
          |> jump keys
          |> walk keys
          |> physics dt
    in
      { gameState | mario = mario}
          |> handleAnyCollisions
          |> Debug.watch "gameState"


jump : Keys -> Model -> Model
jump keys mario =
    if keys.y > 0 && mario.vy == 0
      then { mario | vy = 6.0 }
      else mario


gravity : Float -> Model -> Model
gravity dt mario =
    { mario |
        vy = if mario.y > 0 then mario.vy - dt/4 else 0
    }


physics : Float -> Model -> Model
physics dt mario =
    { mario |
        x = mario.x + dt * mario.vx,
        y = max 0 (mario.y + dt * mario.vy)
    }


walk : Keys -> Model -> Model
walk keys mario =
    { mario |
        vx = toFloat keys.x * 2,
        dir =
          if keys.x < 0 then
            Left
          else if keys.x > 0 then
            Right
          else
            mario.dir
    }


handleAnyCollisions : GameState -> GameState
handleAnyCollisions gameState =
  case isCollision gameState of
    Just BurgerCollision ->
        let burger = gameState.burger
        in
            { gameState |
                burger = { burger | x = burger.x - 100 },
                score = gameState.score + 1 }
    _ -> gameState


isCollision : GameState -> Maybe Collision
isCollision gameState =
  let marioX = gameState.mario.x
      marioY = gameState.mario.y
      burgerX = gameState.burger.x
      burgerY = gameState.burger.y
  in
    if marioX >= burgerX - 20
        && marioX <= burgerX + 20
        && marioY <= burgerY + 20 then
      Just BurgerCollision
    else
      Nothing


-- VIEW

view : (Int, Int) -> GameState -> Element
view (w',h') gameState =
  let (w,h) = (toFloat w', toFloat h')

      mario = gameState.mario

      burger = gameState.burger

      verb =
        if  mario.y > 0 then
          "jump"
        else if mario.vx /= 0 then
          "walk"
        else
          "stand"

      dir =
        case mario.dir of
          Left -> "left"
          Right -> "right"

      hoff =
        case verb of
          "jump" -> "hoff-jump"
          "walk" -> "hoff-walk"
          _      -> "hoff"

      src = "imgs/" ++ hoff ++ "-" ++ dir ++ ".gif"

      marioImage = image 65 65 src

      groundY = 62 - h/2

      position = (mario.x, mario.y + groundY)

      burgerImage = image 25 25 "imgs/burger.png"
      burgerPosition = (burger.x, burger.y + groundY)

      bgImage w h =
        image w h "imgs/background/bg.png"
      bgPosition = (bg.x, bg.y)

      zombieImage = image 65 65 "imgs/zombie-left.png"
      zombiePosition = (zombie.x, zombie.y + groundY)
  in
      collage w' h'
          [ bgImage (round w) (round h)
              |> toForm
              |> move bgPosition

          , marioImage
              |> toForm
              |> Debug.trace "mario"
              |> move position
          , burgerImage
              |> toForm
              |> Debug.trace "burger"
              |> move burgerPosition
          , zombieImage
              |> toForm
              |> Debug.trace "zombie"
              |> move zombiePosition
          , gameState.score
              |> show
              |> toForm
              |> move (500, 300)
          ]


-- SIGNALS

main : Signal Element
main =
  Signal.map2 view Window.dimensions (Signal.foldp update gameState input)


input : Signal (Float, Keys)
input =
  let delta = Signal.map (\t -> t/20) (fps 30)
      deltaArrows =
          Signal.map2 (,) delta (Signal.map (Debug.watch "arrows") Keyboard.arrows)
  in
      Signal.sampleOn delta deltaArrows
