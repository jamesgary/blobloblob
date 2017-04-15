module Main exposing (main)

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
    ( { playerPos = ( 400, 225 )
      , arenaSize = ( 800, 450 )
      , vel = ( 0, 0 )
      , fireCooldown = 0
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
            [ { pos = ( 200, 200 ) }
            , { pos = ( 100, 300 ) }
            , { pos = ( 300, 200 ) }
            , { pos = ( 600, 100 ) }
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
            ( (updateAnimFrame model time), Cmd.none )


updateAnimFrame : Model -> Time.Time -> Model
updateAnimFrame model time =
    model
        |> movePlayer time
        |> fireBullets time
        |> moveBullets time
        |> checkCollisions


checkCollisions : Model -> Model
checkCollisions model =
    let
        ( remainingSpawns, remainingBullets ) =
            collideSpawnsAndBullets model.spawns model.bullets
    in
        { model
            | spawns = remainingSpawns
            , bullets = remainingBullets
        }


collideSpawnsAndBullets : List Spawn -> List Bullet -> ( List Spawn, List Bullet )
collideSpawnsAndBullets spawns bullets =
    case spawns of
        spawn :: otherSpawns ->
            let
                ( maybeSpawn, remainingBullets ) =
                    collideSpawnAndBullets spawn bullets

                ( remainingS, remainingB ) =
                    (collideSpawnsAndBullets otherSpawns remainingBullets)

                actualRemainingS =
                    case maybeSpawn of
                        Just s ->
                            s :: remainingS

                        Nothing ->
                            remainingS
            in
                ( actualRemainingS, remainingB )

        [] ->
            ( [], bullets )


collideSpawnAndBullets : Spawn -> List Bullet -> ( Maybe Spawn, List Bullet )
collideSpawnAndBullets spawn bullets =
    case bullets of
        bullet :: otherBullets ->
            if collideSpawnAndBullet spawn bullet then
                ( Nothing, otherBullets )
            else
                let
                    ( s, remainingBullets ) =
                        collideSpawnAndBullets spawn otherBullets
                in
                    ( s, bullet :: remainingBullets )

        [] ->
            ( Just spawn, [] )


collideSpawnAndBullet : Spawn -> Bullet -> Bool
collideSpawnAndBullet spawn bullet =
    -- is a^2 + b^2 > (rad1 + rad2)^2?
    let
        ( spawnX, spawnY ) =
            spawn.pos

        ( bulletX, bulletY ) =
            bullet.pos

        a =
            spawnX - bulletX

        b =
            spawnY - bulletY

        c =
            spawnRad + bulletRad
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
            model.playerPos

        ( velX, velY ) =
            model.vel

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
            | playerPos = clampedPos
            , vel = newVel
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
                    ({ pos = model.playerPos
                     , angle = angle
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


moveBullets : Time.Time -> Model -> Model
moveBullets time model =
    { model
        | bullets = List.filterMap (moveBullet time model.arenaSize) model.bullets
    }


moveBullet : Time.Time -> Pos -> Bullet -> Maybe Bullet
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
