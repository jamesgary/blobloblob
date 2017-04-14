module Common exposing (..)

import Time


playerRad =
    30


bulletRad =
    10


spawnRad =
    50


bulletFireCooldown =
    100


bulletSpeed =
    15.0


type alias Model =
    { playerPos : ( Float, Float )
    , vel : ( Float, Float )
    , arenaSize : ( Float, Float )
    , bullets : List Bullet
    , fireCooldown : Time.Time
    , currentInputs :
        { isMovingUp : Bool
        , isMovingRight : Bool
        , isMovingDown : Bool
        , isMovingLeft : Bool
        , isFiringUp : Bool
        , isFiringRight : Bool
        , isFiringDown : Bool
        , isFiringLeft : Bool
        }
    , spawns : List Spawn
    }


type alias Spawn =
    { pos : ( Float, Float )
    }


type alias Bullet =
    { pos : ( Float, Float )
    , angle : Float
    }


type Msg
    = KeyDown Int
    | KeyUp Int
    | AnimFrame Time.Time


type Input
    = Move Dir
    | Fire Dir
    | Noop


type Dir
    = Up
    | Right
    | Down
    | Left
