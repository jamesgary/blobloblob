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
        xDiff =
            if model.isMovingRight then
                1
            else if model.isMovingLeft then
                -1
            else
                0

        yDiff =
            if model.isMovingUp then
                -1
            else if model.isMovingDown then
                1
            else
                0

        ( x, y ) =
            model.playerPos

        newPos =
            ( (x + xDiff), (y + yDiff) )
    in
        ({ model
            | playerPos =
                newPos
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
