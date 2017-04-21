module Common exposing (..)

import Time


conf =
    { player =
        { maxHealth = 1000
        , rad = 30
        , fireCooldown = 100
        }
    , bullet =
        { dmg = 5
        , rad = 10
        , speed = 15
        }
    , spawn =
        { rad = 50
        , maxHealth = 100
        , cooldown = 10000
        }
    , minion =
        { rad = 20
        , maxHealth = 20
        }
    }


type alias Model =
    { player : Player
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
    , minions : List Minion
    , spawnCooldown : Time.Time
    }


type alias Player =
    { pos : Pos
    , vel : ( Float, Float )
    , health : Float
    , rad : Float
    }


type alias Minion =
    { pos : Pos
    , vel : ( Float, Float )
    , rad : Float
    , health : Float
    }


type alias Pos =
    ( Float, Float )


type alias Spawn =
    { pos : Pos
    , health : Float
    , rad : Float
    }


type alias Bullet =
    { pos : Pos
    , angle : Angle
    , rad : Float
    , health : Float
    }


type alias Angle =
    Float


type alias Collidable a =
    { a | pos : Pos, rad : Float, health : Float }


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
