module Common exposing (..)

import Time


type alias Model =
    { playerPos : ( Float, Float )
    , playerRad : Float
    , isMovingUp : Bool
    , isMovingRight : Bool
    , isMovingDown : Bool
    , isMovingLeft : Bool
    , vel : ( Float, Float )
    , arenaSize : ( Float, Float )
    }


type Msg
    = KeyDown Int
    | KeyUp Int
    | AnimFrame Time.Time


type Dir
    = Up
    | Right
    | Down
    | Left
    | None
