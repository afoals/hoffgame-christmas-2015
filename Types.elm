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

type Status = Dead | Alive Int

type alias GameState =
    { mario : Model
    , burger : Burger
    , zombie : Model
    , score : Int
    , beach : Beach
    , status : Status
    }

type Direction = Left | Right

