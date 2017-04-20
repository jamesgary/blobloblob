module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Svg exposing (svg, circle)
import Svg.Attributes as SvgAttr
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
            , viewMinions model
            ]


viewPlayer : Model -> Html Msg
viewPlayer model =
    viewObject "player" model.playerPos playerRad


viewObject : String -> Pos -> Float -> Html Msg
viewObject className pos rad =
    let
        ( x, y ) =
            adjustPos pos (-1 * rad)
    in
        div
            [ class className
            , style
                [ ( "transform", translate x y )
                , ( "width", px (2 * rad) )
                , ( "height", px (2 * rad) )
                ]
            ]
            []


adjustPos : Pos -> Float -> Pos
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


viewMinions : Model -> Html Msg
viewMinions model =
    div [ class "minions" ] (List.map viewMinion model.minions)


viewMinion : Minion -> Html Msg
viewMinion minion =
    viewObject "minion" minion.pos minionRad


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
    let
        ( x, y ) =
            spawn.pos
    in
        div
            [ class "spawn-container"
            , style
                [ ( "transform", translate x y )
                , ( "width", px (2 * spawnRad) )
                , ( "height", px (2 * spawnRad) )
                ]
            ]
            [ viewSpawnSprite
            , viewHealth spawn.health
            ]


viewSpawnSprite : Html Msg
viewSpawnSprite =
    div [ class "spawn" ] []


viewHealth : Float -> Html Msg
viewHealth health =
    let
        perc =
            1.0 - (health / spawnMaxHealth)

        sda =
            (toString (50 * perc * pi)) ++ "% 9999%"
    in
        svg [ SvgAttr.class "health-container" ]
            [ circle [ SvgAttr.class "health-bg" ] []
            , circle
                [ SvgAttr.class "health-dmg"
                , SvgAttr.strokeDasharray sda
                ]
                []
            ]


translate : Float -> Float -> String
translate x y =
    "translate(" ++ (px x) ++ "," ++ (px y) ++ ")"


px : Float -> String
px num =
    (toString num) ++ "px"


toPerc : Float -> String
toPerc num =
    (toString num) ++ "%"
