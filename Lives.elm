import Types exposing (..)

module Lives where

loose : GameState -> GameState
loose gs =
    case gs.status of
        Alive livesLeft ->
            let newStatus = if livesLeft > 1 then Alive (livesLeft - 1) else Dead
                in { gs | status = newStatus }
        _ -> gs

