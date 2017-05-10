module Render exposing (renderArena)

import Html exposing (Html)
import Color
import Math.Vector2 as Vector2


-- elm-2d

import Game.TwoD.Camera as Camera exposing (Camera)
import Game.TwoD.Render as Render exposing (Renderable, rectangle, triangle, circle)
import Game.TwoD as Game


-- mine

import Common exposing (..)


renderArena : Model -> Html Msg
renderArena model =
    let
        ( arenaWidth, arenaHeight ) =
            model.arenaSize

        arenaWidthInt =
            round arenaWidth

        arenaHeightInt =
            round arenaHeight
    in
        Game.renderCentered
            { time = 0
            , camera = Camera.fixedArea (arenaWidth * arenaHeight) ( arenaWidth / 2, arenaHeight / 2 )
            , size = ( arenaWidthInt, arenaHeightInt )
            }
            ([ renderPlayer model.player ]
                ++ (List.map renderSpawn model.spawns)
                ++ (List.map renderBullet model.bullets)
                ++ (List.map renderMinion model.minions)
            )


renderPlayer : Player -> Renderable
renderPlayer player =
    renderCircle player.pos conf.player.rad Color.green


renderMinion : Minion -> Renderable
renderMinion minion =
    renderCircle minion.pos conf.minion.rad Color.purple


renderBullet : Bullet -> Renderable
renderBullet bullet =
    renderCircle bullet.pos conf.bullet.rad Color.yellow


renderSpawn : Spawn -> Renderable
renderSpawn spawn =
    renderCircle spawn.pos conf.spawn.rad Color.gray


renderCircle : Pos -> Float -> Color.Color -> Renderable
renderCircle pos rad color =
    let
        ( x, y ) =
            Vector2.toTuple pos
    in
        Render.shape circle
            { color = color
            , position = ( x - rad, y - rad )
            , size = ( rad * 2, rad * 2 )
            }
