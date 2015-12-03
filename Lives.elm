module Lives where
import Types exposing (..)

loose : Status -> Status
loose gs =
    case gs of
        Alive livesLeft ->
            if livesLeft > 1 then 
                let newLives = livesLeft - 1
                    in Hurt { livesLeft = newLives, timeLeft = 80 }
            else Dead
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

