module Common exposing (..)

import Time
import Math.Vector2 exposing (Vec2)


conf =
    { player =
        { maxHealth = 1000
        , rad = 10
        , fireCooldown = 100
        , speed = 0.1
        , friction = 0.77
        }
    , bullet =
        { dmg = 5
        , rad = 3
        , speed = 15
        }
    , spawn =
        { rad = 30
        , maxHealth = 100
        , cooldown = 200
        }
    , minion =
        { rad = 10
        , maxHealth = 5
        , dmg = 100
        , speed = 0.08
        , friction = 0.99
        }
    , effects =
        { maxAge = 300
        , rad = 10
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
    , effects : List Effect
    }


type alias Effect =
    { pos : Pos
    , age : Time.Time
    , type_ : EffectType
    }


type EffectType
    = BulletHit
    | MinionDeath
    | SpawnDeath


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
    Vec2


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
