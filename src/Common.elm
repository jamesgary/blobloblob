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
    , isFiring : Bool
    , bullets : List Bullet
    , fireCooldown : Time.Time
    }


type alias Bullet =
    { pos : ( Float, Float )
    , dir : Dir
    }


type Msg
    = KeyDown Int
    | KeyUp Int
    | AnimFrame Time.Time


type Input
    = MoveUp
    | MoveRight
    | MoveDown
    | MoveLeft
    | Fire
    | None


type Dir
    = Up
    | Right
    | Down
    | Left
