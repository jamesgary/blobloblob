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
      , isMovingUp = False
      , isMovingRight = False
      , isMovingDown = False
      , isMovingLeft = False
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

        newPos =
            ( ((time * newVelX * friction) + posX)
            , ((time * newVelY * friction) + posY)
            )

        newVel =
            ( newVelX, newVelY )
    in
        ({ model
            | playerPos = newPos
            , vel = newVel
         }
        )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        , AnimationFrame.diffs AnimFrame
        ]
