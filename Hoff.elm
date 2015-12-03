import Color exposing (..)
import Debug
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (..)
import Window
import Random
import Zombie
import Types exposing (..)
import Lives


-- MODEL


type alias Keys = { x:Int, y:Int }

type Collision = BurgerCollision | ZombieCollision

type alias WindowDimensions = { height: Int, width: Int }


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

beach2 : Beach
beach2 =
  { x = 0
  , y = 0
  , vx = 0
  , dir = Left
  }

beach3 : Beach
beach3 =
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
    , status = (Alive 5)
    , beach2 = beach2
    , beach3 = beach3
    }


-- UPDATE

tryUpdate : (Float, Keys, WindowDimensions) -> GameState -> GameState
tryUpdate (dt, keys, dimensions) gameState =
  case gameState.status of
    Dead -> gameState
    _    -> update (dt, keys, dimensions) gameState

update : (Float, Keys, WindowDimensions) -> GameState -> GameState
update (dt, keys, dimensions) gameState =
    let mario =
            gameState.mario
            |> gravity dt
            |> jump keys
            |> walk keys
            |> physics dt dimensions

        burger =
            gameState.burger
            |> moveBurger keys dt
        zombie =
            gameState.zombie
            |> moveX keys
        beach =
            gameState.beach
            |> scrollBg keys
            |> beachPhysics dt dimensions
        beach2 =
            gameState.beach2
            |> scrollBg keys
            |> beachPhysics dt dimensions
        beach3 =
            gameState.beach3
            |> scrollBg keys
            |> beachPhysics dt dimensions
        newStatus =
            Lives.update gameState.status

    in
      { gameState | mario = mario, zombie = zombie, beach = beach, beach2 = beach2, beach3 = beach3, status = newStatus, burger = burger }
          |> handleAnyCollisions
          |> Debug.watch "gameState"


moveX keys zombie =
    let vx = toFloat keys.x * 6
        new = if (zombie.x <= -400.0) then 400 else zombie.x - vx - 10
    in { zombie | x = new }

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


physics : Float -> WindowDimensions -> Model -> Model
physics dt dimensions mario =
    { mario |
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

beachPhysics : Float -> WindowDimensions -> Beach -> Beach
beachPhysics dt dimensions beach =
    let w = toFloat dimensions.width
        newX = beach.x - dt * beach.vx
    in { beach |
        x =
          if (beach.dir == Right) then
            if (newX <= 1 - w) then 0 else newX
          else if (newX >= w) then 0 else newX
    }

moveBurger : Keys -> Float -> Burger -> Burger
moveBurger keys dt burger =
  let vx = toFloat keys.x * 3
      dir =
        if keys.x < 0 then
          Left
        else
          Right
      newX = burger.x - dt * vx
  in
    { burger | x = newX }


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
                zombie = { zombie | x = zombie.x + 1000 }
            ,   status = (Lives.loose gameState.status) }
    Nothing -> gameState


isCollision : GameState -> Maybe Collision
isCollision gameState =
  case gameState.status of
    Dead -> Nothing
    Hurt _ -> Nothing
    Alive _ ->
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
fadeIfHurt status element =
  case status of
    Hurt _ -> opacity 0.3 element
    _ -> element


view : (Int, Int) -> GameState -> Element
view (w',h') gameState =
  let (w,h) = (toFloat w', toFloat h')

      mario = gameState.mario

      burger = gameState.burger

      zombie = gameState.zombie

      beach = gameState.beach

      beach2 = gameState.beach2

      beach3 = gameState.beach3

      hoffSrc =
        case gameState.status of
          Dead  -> "imgs/hoff-explode.png"
          _     -> let
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
                   in "imgs/" ++ hoff ++ "-" ++ dir ++ ".gif"

      marioImage = image 150 150 hoffSrc

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

      beach2Image w h =
        image w h "imgs/background/beach.png"
      beach2Position = (beach2.x + w, beach2.y)

      beach3Image w h =
        image w h "imgs/background/beach.png"
      beach3Position = (beach3.x - w, beach3.y)

      zombieImage = image 150 150 "imgs/zombie-default.gif"
      zombiePosition = (zombie.x, zombie.y + groundY + 50)

      lives =
        case gameState.status of
          Alive l -> l
          Hurt h  -> h.livesLeft
          Dead    -> 0
      livesImage = image 35 35 "imgs/hoff-right.gif"
  in
      collage w' h'
          [ skyImage (round w) (round h)
              |> toForm
              |> move skyPosition
          , beachImage (round w) (round h)
              |> toForm
              |> move beachPosition
          , beach2Image (round w) (round h)
              |> toForm
              |> move beach2Position
          , beach3Image (round w) (round h)
              |> toForm
              |> move beach3Position
          , marioImage
              |> fadeIfHurt gameState.status
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
          , livesImage
              |> toForm
              |> move (474, 300)
          , lives
              |> show
              |> toForm
              |> move (500, 300)
          , burgerImage
              |> toForm
              |> move (470, 260)
          , gameState.score
              |> show
              |> toForm
              |> move (500, 260)
          ]


-- SIGNALS

main : Signal Element
main =
  Signal.map2 view Window.dimensions (Signal.foldp tryUpdate gameState input)


input : Signal (Float, Keys, WindowDimensions)
input =
  let delta = Signal.map (\t -> t/10) (fps 30)
      deltaArrows =
          Signal.map2 (,) delta (Signal.map (Debug.watch "arrows") Keyboard.arrows)
  in
      Signal.sampleOn delta deltaArrows
      |> Signal.map2 (\(w,h) (f, ks) -> (f, ks, { height = h, width = w })) Window.dimensions
