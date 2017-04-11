module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random
import Keyboard
import Json.Decode as Json
import AnimationFrame
import Time


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { playerPos : ( Float, Float )
    , isMovingUp : Bool
    , isMovingRight : Bool
    , isMovingDown : Bool
    , isMovingLeft : Bool
    , arenaSize : ( Float, Float )
    }


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


type Msg
    = KeyDown Int
    | KeyUp Int
    | AnimFrame Time.Time


upKeyCode =
    87


rightKeyCode =
    68


downKeyCode =
    83


leftKeyCode =
    65


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown key ->
            ( (updateKeyDown model key), Cmd.none )

        KeyUp key ->
            ( (updateKeyUp model key), Cmd.none )

        AnimFrame time ->
            ( (updateAnimFrame model time), Cmd.none )


updateKeyDown : Model -> Int -> Model
updateKeyDown model key =
    case key of
        87 ->
            { model | isMovingUp = True }

        68 ->
            { model | isMovingRight = True }

        83 ->
            { model | isMovingDown = True }

        65 ->
            { model | isMovingLeft = True }

        _ ->
            model


updateKeyUp : Model -> Int -> Model
updateKeyUp model key =
    case key of
        87 ->
            { model | isMovingUp = False }

        68 ->
            { model | isMovingRight = False }

        83 ->
            { model | isMovingDown = False }

        65 ->
            { model | isMovingLeft = False }

        _ ->
            model


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



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ class "game" ]
        [ viewArena model
        ]


viewArena : Model -> Html Msg
viewArena model =
    let
        ( width, height ) =
            model.arenaSize
    in
        div
            [ class "arena"
            , style
                [ ( "width", px width )
                , ( "height", px height )
                ]
            ]
            [ viewPlayer model
            ]


viewPlayer : Model -> Html Msg
viewPlayer model =
    let
        ( x, y ) =
            model.playerPos
    in
        div
            [ class "player"
            , style
                [ ( "transform", translate x y )
                ]
            ]
            []


translate : Float -> Float -> String
translate x y =
    "translate(" ++ (px x) ++ "," ++ (px y) ++ ")"


px : Float -> String
px num =
    (toString num) ++ "px"
