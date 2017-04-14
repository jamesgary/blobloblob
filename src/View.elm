module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Keyboard
import Json.Decode as Json


-- mine

import Common exposing (..)


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
            , viewSpawns model
            , viewBullets model
            ]


viewPlayer : Model -> Html Msg
viewPlayer model =
    viewObject "player" model.playerPos playerRad


viewObject : String -> ( Float, Float ) -> Float -> Html Msg
viewObject className pos rad =
    let
        ( x, y ) =
            adjustPos pos (-0.5 * rad)
    in
        div
            [ class className
            , style
                [ ( "transform", translate x y )
                , ( "width", px rad )
                , ( "height", px rad )
                ]
            ]
            []


adjustPos : ( Float, Float ) -> Float -> ( Float, Float )
adjustPos pos adj =
    let
        ( x, y ) =
            pos

        adjustedX =
            x + adj

        adjustedY =
            y + adj
    in
        ( adjustedX, adjustedY )


viewBullets : Model -> Html Msg
viewBullets model =
    div [ class "bullets" ] (List.map viewBullet model.bullets)


viewBullet : Bullet -> Html Msg
viewBullet bullet =
    viewObject "bullet" bullet.pos bulletRad


viewSpawns : Model -> Html Msg
viewSpawns model =
    div [ class "spawns" ] (List.map viewSpawn model.spawns)


viewSpawn : Spawn -> Html Msg
viewSpawn spawn =
    viewObject "spawn" spawn.pos spawnRad


translate : Float -> Float -> String
translate x y =
    "translate(" ++ (px x) ++ "," ++ (px y) ++ ")"


px : Float -> String
px num =
    (toString num) ++ "px"
