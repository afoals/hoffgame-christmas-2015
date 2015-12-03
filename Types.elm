module Types where

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

type alias Hurting =
    { livesLeft : Int
    , timeLeft : Int
    }

type Status = Dead | Alive Int | Hurt Hurting

type alias GameState =
    { mario : Model
    , burgers : List Burger
    , zombie : Model
    , zombie2 : Model
    , score : Int
    , beach : Beach
    , beach2 : Beach
    , beach3 : Beach
    , status : Status
    }

type Direction = Left | Right
