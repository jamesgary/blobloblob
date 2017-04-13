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
      , playerRad = 52
      , arenaSize = ( 800, 450 )
      , vel = ( 0, 0 )
      , isMovingUp = False
      , isMovingRight = False
      , isMovingDown = False
      , isMovingLeft = False
      , isFiring = False
      , fireCooldown = 0
      , bullets = []
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


movePlayer : Time.Time -> Model -> Model
movePlayer time model =
    let
        xDir =
            if model.isMovingRight then
                1
            else if model.isMovingLeft then
                -1
            else
                0

        yDir =
            if model.isMovingUp then
                -1
            else if model.isMovingDown then
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

        halfRad =
            model.playerRad * 0.5

        clampedPos =
            ( (clamp halfRad (arenaWidth - halfRad) newPosX)
            , (clamp halfRad (arenaHeight - halfRad) newPosY)
            )

        newVel =
            ( newVelX, newVelY )
    in
        ({ model
            | playerPos = clampedPos
            , vel = newVel
         }
        )


bulletFireCooldown =
    100


fireBullets : Time.Time -> Model -> Model
fireBullets time model =
    let
        fireCooldown =
            model.fireCooldown - time

        shouldFire =
            (model.isFiring && fireCooldown <= 0)
    in
        if shouldFire then
            { model
                | fireCooldown = bulletFireCooldown
                , bullets =
                    ({ pos = model.playerPos
                     , dir = Left
                     }
                        :: model.bullets
                    )
            }
        else
            { model
                | fireCooldown = fireCooldown
            }


moveBullets : Time.Time -> Model -> Model
moveBullets time model =
    { model
        | bullets = List.map (moveBullet time) model.bullets
    }


bulletSpeed =
    5


moveBullet : Time.Time -> Bullet -> Bullet
moveBullet time bullet =
    let
        ( xDir, yDir ) =
            case bullet.dir of
                Up ->
                    ( 0, -1 )

                Right ->
                    ( 1, 0 )

                Down ->
                    ( 1, 0 )

                Left ->
                    ( -1, 0 )

        ( x, y ) =
            bullet.pos

        newX =
            x + (xDir * bulletSpeed)

        newY =
            y + (yDir * bulletSpeed)
    in
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
