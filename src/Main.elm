port module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random
import Keyboard
import Json.Decode as Json
import AnimationFrame
import Time


-- mine

import Common exposing (..)
import Input exposing (..)
import View exposing (view)


port raf : () -> Cmd msg


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


init : ( Model, Cmd Msg )
init =
    ( { player =
            { pos = ( 400, 225 )
            , vel = ( 0, 0 )
            }
      , arenaSize = ( 800, 450 )
      , fireCooldown = 0
      , spawnCooldown = 0
      , bullets = []
      , currentInputs =
            { isMovingUp = False
            , isMovingRight = False
            , isMovingDown = False
            , isMovingLeft = False
            , isFiringUp = False
            , isFiringRight = False
            , isFiringDown = False
            , isFiringLeft = False
            }
      , spawns =
            [ { pos = ( 50, 50 ), health = spawnMaxHealth, rad = spawnRad }

            --j, { pos = ( 150, 50 ), health = spawnMaxHealth, rad = spawnRad }
            --j, { pos = ( 350, 50 ), health = spawnMaxHealth, rad = spawnRad }
            --j, { pos = ( 550, 50 ), health = spawnMaxHealth, rad = spawnRad }
            --j, { pos = ( 750, 50 ), health = spawnMaxHealth, rad = spawnRad }
            --j, { pos = ( 50, 400 ), health = spawnMaxHealth, rad = spawnRad }
            , { pos = ( 750, 400 ), health = spawnMaxHealth, rad = spawnRad }
            ]
      , minions =
            [ { pos = ( 300, 200 )
              , vel = ( 1, 2 )
              , rad = minionRad
              , health = minionMaxHealth
              }
            ]
      }
    , Cmd.none
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown key ->
            ( (updateKeyDown model key), Cmd.none )

        KeyUp key ->
            ( (updateKeyUp model key), Cmd.none )

        AnimFrame time ->
            ( (updateAnimFrame model time), raf () )


updateAnimFrame : Model -> Time.Time -> Model
updateAnimFrame model time =
    model
        -- player stuff
        |> movePlayer time
        -- minion stuff
        |> spawnMinions time
        |> moveMinions time
        -- bullet stuff
        |> fireBullets time
        |> moveBullets time
        -- other
        |> checkCollisions
        |> removeDead


spawnMinions : Time.Time -> Model -> Model
spawnMinions time model =
    let
        currentCooldown =
            model.spawnCooldown - time
    in
        if currentCooldown < 0 then
            { model
                | spawnCooldown = spawnMinionCooldown -- reset
                , minions = List.append (List.map spawnMinion model.spawns) model.minions
            }
        else
            { model
                | spawnCooldown = currentCooldown
            }


spawnMinion : Spawn -> Minion
spawnMinion spawn =
    { pos = spawn.pos
    , vel = ( 1, 2 )
    , rad = minionRad
    , health = minionMaxHealth
    }


removeDead : Model -> Model
removeDead model =
    { model
        | spawns = List.filter isAlive model.spawns
        , minions = List.filter isAlive model.minions
    }


isAlive : Collidable a -> Bool
isAlive spawn =
    spawn.health > 0


checkCollisions : Model -> Model
checkCollisions model =
    let
        ( spawns, bullets ) =
            List.foldr collideObjWithObjs ( [], model.bullets ) model.spawns

        ( minions, bullets2 ) =
            List.foldr collideObjWithObjs ( [], bullets ) model.minions

        ( player, minions2 ) =
            collidePlayerWithMinions model.player minions
    in
        { model
            | spawns = spawns
            , minions = minions2
            , bullets = bullets2
        }


collidePlayerWithMinions : Player -> List Minion -> ( Player, List Minion )
collidePlayerWithMinions player minions =
    ( player, minions )


collideObjWithObjs :
    Collidable a
    -> ( List (Collidable a), List (Collidable b) )
    -> ( List (Collidable a), List (Collidable b) )
collideObjWithObjs obj1 ( obj2s, obj1s ) =
    case obj1s of
        obj :: otherObjs ->
            if collideObjects obj1 obj then
                ( { obj1 | health = obj1.health - bulletDmg } :: obj2s, otherObjs )
            else
                let
                    ( newSpawns, newBullets ) =
                        collideObjWithObjs obj1 ( obj2s, otherObjs )
                in
                    ( newSpawns, obj :: newBullets )

        [] ->
            ( obj1 :: obj2s, obj1s )


collideObjects : Collidable a -> Collidable b -> Bool
collideObjects obj1 obj2 =
    -- is a^2 + b^2 > (rad1 + rad2)^2?
    let
        ( x1, y1 ) =
            obj1.pos

        ( x2, y2 ) =
            obj2.pos

        a =
            x1 - x2

        b =
            y1 - y2

        c =
            obj1.rad + obj2.rad
    in
        (a ^ 2) + (b ^ 2) < (c ^ 2)


movePlayer : Time.Time -> Model -> Model
movePlayer time model =
    let
        xDir =
            if model.currentInputs.isMovingRight then
                1
            else if model.currentInputs.isMovingLeft then
                -1
            else
                0

        yDir =
            if model.currentInputs.isMovingUp then
                -1
            else if model.currentInputs.isMovingDown then
                1
            else
                0

        ( posX, posY ) =
            model.player.pos

        ( velX, velY ) =
            model.player.vel

        c =
            0.15

        friction =
            0.8

        ( newVelX, newVelY ) =
            ( ((velX + (xDir * c)) * friction)
            , ((velY + (yDir * c)) * friction)
            )

        ( newPosX, newPosY ) =
            ( ((time * newVelX * friction) + posX)
            , ((time * newVelY * friction) + posY)
            )

        ( arenaWidth, arenaHeight ) =
            model.arenaSize

        clampedPos =
            ( (clamp playerRad (arenaWidth - playerRad) newPosX)
            , (clamp playerRad (arenaHeight - playerRad) newPosY)
            )

        newVel =
            ( newVelX, newVelY )
    in
        ({ model
            | player =
                { pos = clampedPos
                , vel = newVel
                }
         }
        )


fireBullets : Time.Time -> Model -> Model
fireBullets time model =
    let
        fireCooldown =
            model.fireCooldown - time

        wantToFire =
            (model.currentInputs.isFiringUp
                || model.currentInputs.isFiringRight
                || model.currentInputs.isFiringDown
                || model.currentInputs.isFiringLeft
            )

        shouldFire =
            (wantToFire && fireCooldown <= 0)

        angle =
            fromDirsGetAngle
                model.currentInputs.isFiringUp
                model.currentInputs.isFiringRight
                model.currentInputs.isFiringDown
                model.currentInputs.isFiringLeft
    in
        if shouldFire then
            { model
                | fireCooldown = bulletFireCooldown
                , bullets =
                    ({ pos = model.player.pos
                     , angle = angle
                     , rad = bulletRad
                     , health = 1
                     }
                        :: model.bullets
                    )
            }
        else
            { model
                | fireCooldown = fireCooldown
            }


fromDirsGetAngle : Bool -> Bool -> Bool -> Bool -> Angle
fromDirsGetAngle up right down left =
    turns <|
        case ( up, right, down, left ) of
            ( True, True, _, _ ) ->
                0.875

            ( True, _, _, True ) ->
                0.625

            ( _, _, True, True ) ->
                0.375

            ( _, True, True, _ ) ->
                0.125

            ( True, _, _, _ ) ->
                0.75

            ( _, True, _, _ ) ->
                0

            ( _, _, True, _ ) ->
                0.25

            ( _, _, _, True ) ->
                0.5

            _ ->
                0


moveMinions : Time.Time -> Model -> Model
moveMinions time model =
    { model
        | minions = List.map (moveMinion time model.arenaSize) model.minions
    }


moveMinion : Time.Time -> ( Float, Float ) -> Minion -> Minion
moveMinion time ( arenaWidth, arenaHeight ) minion =
    let
        ( xDelta, yDelta ) =
            minion.vel

        ( x, y ) =
            minion.pos

        newX =
            x + xDelta

        newY =
            y + yDelta

        newXDelta =
            if (newX < 0 || newX > arenaWidth) then
                -1 * xDelta
            else
                xDelta

        newYDelta =
            if (newY < 0 || newY > arenaHeight) then
                -1 * yDelta
            else
                yDelta

        newX2 =
            x + newXDelta

        newY2 =
            y + newYDelta
    in
        { minion
            | pos = ( newX2, newY2 )
            , vel = ( newXDelta, newYDelta )
        }


moveBullets : Time.Time -> Model -> Model
moveBullets time model =
    { model
        | bullets = List.filterMap (moveBullet time model.arenaSize) model.bullets
    }


moveBullet : Time.Time -> ( Float, Float ) -> Bullet -> Maybe Bullet
moveBullet time ( arenaWidth, arenaHeight ) bullet =
    let
        ( xDelta, yDelta ) =
            fromPolar ( bulletSpeed, bullet.angle )

        ( x, y ) =
            bullet.pos

        newX =
            x + xDelta

        newY =
            y + yDelta
    in
        if (newX < 0 || newX > arenaWidth || newY < 0 || newY > arenaHeight) then
            Nothing
        else
            Just
                { bullet
                    | pos = ( newX, newY )
                }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        , AnimationFrame.diffs AnimFrame
        ]
