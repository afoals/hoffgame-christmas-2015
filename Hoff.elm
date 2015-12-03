import Color exposing (..)
import Debug
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (..)
import Window
import Random
import Zombie


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

type alias Sky =
    { x : Float
    , y : Float
    }


type alias Beach =
    { x : Float
    , y : Float
    , vx : Float
    , dir : Direction
    }

type alias GameState =
    { mario : Model
    , burger : Burger
    , zombie : Model
    , score : Int
    , beach : Beach
    }

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

sky : Sky
sky =
    { x = 0
    , y = 0
    }

beach : Beach
beach =
  { x = 0
  , y = 0
  , vx = 0
  , dir = Left
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
    , zombie = zombie
    , score = 0
    , beach = beach
    }


-- UPDATE

update : (Float, Keys) -> GameState -> GameState
update (dt, keys) gameState =
    let mario =
            gameState.mario
                |> gravity dt
                |> jump keys
                |> walk keys
                |> physics dt

        zombie =
            gameState.zombie
            |> moveX
        beach =
            gameState.beach
            |> scrollBg keys
    in

      { gameState | mario = mario, zombie = zombie, beach = beach }

          |> handleAnyCollisions
          |> Debug.watch "gameState"


moveX ball =
    let new = if (ball.x <= -400.0) then 400 else ball.x - 3
    in { ball | x = new }

jump : Keys -> Model -> Model
jump keys mario =
    if keys.y > 0 && mario.vy == 0
      then { mario | vy = 10.0 }
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
        vx = toFloat keys.x * 3,
        dir =
          if keys.x < 0 then
            Left
          else if keys.x > 0 then
            Right
          else
            mario.dir
    }

beachPhysics : Float -> Beach -> Beach
beachPhysics dt beach =
    { beach |
        x = beach.x + dt * beach.vx
    }


scrollBg : Keys -> Beach -> Beach
scrollBg keys beach =
    { beach |
        vx = toFloat keys.x * 3,
        dir =
          if keys.x < 0 then
            Left
          else if keys.x > 0 then
            Right
          else
            beach.dir
    }

handleAnyCollisions : GameState -> GameState
handleAnyCollisions gameState =
  case isCollision gameState of
    Just BurgerCollision ->
        let burger = gameState.burger
            zombie = gameState.zombie
        in
            { gameState |
                burger = { burger | x = burger.x - 100 },
                score = gameState.score + 1 }
    Just ZombieCollision ->
        let zombie = gameState.zombie
        in
            { gameState |
                zombie = { zombie | x = zombie.x + 1000 },
                score = gameState.score - 1 }
    Nothing -> gameState


isCollision : GameState -> Maybe Collision
isCollision gameState =
  let marioX = gameState.mario.x
      marioY = gameState.mario.y
      burgerX = gameState.burger.x
      burgerY = gameState.burger.y
      zombieX = gameState.zombie.x
      zombieY = gameState.zombie.y
      collide x1 y1 x2 y2 =
        x1 >= x2 - 20
        && x1 <= x2 + 20
        && y1 <= y2 + 20
  in
    if collide marioX marioY burgerX burgerY then
      Just BurgerCollision
    else if collide marioX marioY zombieX zombieY then
      Just ZombieCollision
    else
      Nothing


-- VIEW

view : (Int, Int) -> GameState -> Element
view (w',h') gameState =
  let (w,h) = (toFloat w', toFloat h')

      mario = gameState.mario

      burger = gameState.burger

      zombie = gameState.zombie

      beach = gameState.beach

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

      marioImage = image 150 150 src

      groundY = 62 - h/2

      position = (mario.x, mario.y + groundY + 50)

      burgerImage = image 25 25 "imgs/burger.png"
      burgerPosition = (burger.x, burger.y + groundY)

      skyImage w h =
        image w h "imgs/background/sky.png"
      skyPosition = (sky.x, sky.y)

      beachImage w h =
        image w h "imgs/background/beach.png"
      beachPosition = (beach.x, beach.y)

      zombieImage = image 150 150 "imgs/zombie-left.png"
      zombiePosition = (zombie.x, zombie.y + groundY + 50)
  in
      collage w' h'
          [ skyImage (round w) (round h)
              |> toForm
              |> move skyPosition
          , beachImage (round w) (round h)
              |> toForm
              |> move beachPosition
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
