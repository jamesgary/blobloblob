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
            , viewBullets model
            , viewSpawns model
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


viewBullets : Model -> Html Msg
viewBullets model =
    div [ class "bullets" ] (List.map viewBullet model.bullets)


viewBullet : Bullet -> Html Msg
viewBullet bullet =
    let
        ( x, y ) =
            bullet.pos
    in
        div
            [ class "bullet"
            , style
                [ ( "transform", translate x y )
                ]
            ]
            []


viewSpawns : Model -> Html Msg
viewSpawns model =
    div [ class "spawns" ] (List.map viewSpawn model.spawns)


viewSpawn : Spawn -> Html Msg
viewSpawn spawn =
    let
        ( x, y ) =
            spawn.pos
    in
        div
            [ class "spawn"
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
