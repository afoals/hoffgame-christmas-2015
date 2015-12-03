module Lives where
import Types exposing (..)

loose : GameState -> GameState
loose gs =
    case gs.status of
        Alive livesLeft ->
            let newStatus = if livesLeft > 1 then Alive (livesLeft - 1) else Dead
                in { gs | status = newStatus }
        _ -> gs

update: Status -> Status
update oldStatus =
    case oldStatus of
        Hurt hurting ->
            if hurting.timeLeft > 0 then
            let timeLeft = hurting.timeLeft - 1
            in Hurt ({ timeLeft = timeLeft, livesLeft = hurting.livesLeft })
            else Alive (hurting.livesLeft)

        _ -> oldStatus

