module Common exposing (..)

import Time


playerRad =
    30


conf =
    { player =
        { maxHealth = 1000
        }
    }


bulletRad =
    10


bulletDmg =
    5


spawnRad =
    50


spawnMaxHealth =
    100


minionRad =
    20


minionMaxHealth =
    20


spawnMinionCooldown =
    10000


bulletFireCooldown =
    100


bulletSpeed =
    15.0


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
