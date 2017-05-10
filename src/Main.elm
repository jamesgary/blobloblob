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
import Collisions exposing (checkCollisions)
import Input exposing (updateKeyDown, updateKeyUp)
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
            { pos = ( 700, 737 )
            , vel = ( 0, 0 )
            , health = 1000
            , rad = conf.player.rad
            }
      , arenaSize = ( 1200, 675 )
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
            [ { pos = ( 350, 350 ), health = conf.spawn.maxHealth, rad = conf.spawn.rad }
            , { pos = ( 350, 600 ), health = conf.spawn.maxHealth, rad = conf.spawn.rad }
            , { pos = ( 750, 400 ), health = conf.spawn.maxHealth, rad = conf.spawn.rad }
            ]
      , minions = []
      , effects = []
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
    if model.player.health > 0 then
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
            |> ageEffects time
            |> removeDead
    else
        model


ageEffects : Time.Time -> Model -> Model
ageEffects time model =
    { model
        | effects = List.filterMap (ageEffect time) model.effects
    }


ageEffect : Time.Time -> Effect -> Maybe Effect
ageEffect time effect =
    let
        newAge =
            effect.age + time
    in
        if newAge > conf.effects.maxAge then
            Nothing
        else
            Just { effect | age = effect.age + time }


spawnMinions : Time.Time -> Model -> Model
spawnMinions time model =
    let
        currentCooldown =
            model.spawnCooldown - time
    in
        if currentCooldown < 0 then
            { model
                | spawnCooldown = conf.spawn.cooldown -- reset
                , minions = List.append (List.map spawnMinion model.spawns) model.minions
            }
        else
            { model
                | spawnCooldown = currentCooldown
            }


spawnMinion : Spawn -> Minion
spawnMinion spawn =
    { pos = spawn.pos
    , vel = ( 0, 0 )
    , rad = conf.minion.rad
    , health = conf.minion.maxHealth
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
                1
            else if model.currentInputs.isMovingDown then
                -1
            else
                0

        player =
            model.player

        ( posX, posY ) =
            player.pos

        ( velX, velY ) =
            player.vel

        ( newVelX, newVelY ) =
            ( ((velX + (xDir * conf.player.speed)) * conf.player.friction)
            , ((velY + (yDir * conf.player.speed)) * conf.player.friction)
            )

        ( newPosX, newPosY ) =
            ( ((time * newVelX * conf.player.friction) + posX)
            , ((time * newVelY * conf.player.friction) + posY)
            )

        ( arenaWidth, arenaHeight ) =
            model.arenaSize

        clampedPos =
            ( (clamp conf.player.rad (arenaWidth - conf.player.rad) newPosX)
            , (clamp conf.player.rad (arenaHeight - conf.player.rad) newPosY)
            )

        newVel =
            ( newVelX, newVelY )

        newPlayer =
            { player
                | pos = clampedPos
                , vel = newVel
            }
    in
        { model | player = newPlayer }


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
                | fireCooldown = conf.player.fireCooldown
                , bullets =
                    ({ pos = model.player.pos
                     , angle = angle
                     , rad = conf.bullet.rad
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
                0.125

            ( True, _, _, True ) ->
                0.375

            ( _, _, True, True ) ->
                0.625

            ( _, True, True, _ ) ->
                0.875

            ( True, _, _, _ ) ->
                0.25

            ( _, True, _, _ ) ->
                0

            ( _, _, True, _ ) ->
                0.75

            ( _, _, _, True ) ->
                0.5

            _ ->
                0


moveMinions : Time.Time -> Model -> Model
moveMinions time model =
    let
        movedMinions =
            List.map (moveMinion time model.player.pos model.arenaSize) model.minions

        collidedMinions =
            List.foldr collideMMs [] movedMinions
    in
        { model | minions = collidedMinions }


collideMMs : Minion -> List Minion -> List Minion
collideMMs minion minions =
    case minions of
        m :: otherMinions ->
            let
                ( m1, m2 ) =
                    (collideMM minion m)

                collidedOMs =
                    collideMMs m1 (otherMinions)
            in
                m2 :: collidedOMs

        [] ->
            [ minion ]


collideMM : Minion -> Minion -> ( Minion, Minion )
collideMM m1 m2 =
    --let
    --    ( x1, y1 ) =
    --        m1.pos
    --    ( x2, y2 ) =
    --        m2.pos
    --    dist =
    --        sqrt (((x1 - x2) ^ 2) + ((y1 - y2) ^ 2))
    --in
    --    if dist < (conf.minion.rad * 2) then
    --        let
    --            ( vx1, vy1 ) =
    --                m1.vel
    --            ( vx2, vy2 ) =
    --                m2.vel
    --            nx =
    --                (x2 - x1) / dist
    --            ny =
    --                (y2 - y1) / dist
    --            p =
    --                2.5 * (vx1 * nx + vy1 * ny - vx2 * nx - vy2 * ny) / (2)
    --            newM1 =
    --                { m1 | vel = ( vx1 - p * nx, vy1 - p * ny ) }
    --            newM2 =
    --                { m2 | vel = ( vx2 + p * nx, vy2 + p * ny ) }
    --        in
    --            ( newM1, newM2 )
    --    else
    ( m1, m2 )


moveMinion : Time.Time -> Pos -> ( Float, Float ) -> Minion -> Minion
moveMinion time ( playerPosX, playerPosY ) ( arenaWidth, arenaHeight ) minion =
    let
        ( posX, posY ) =
            minion.pos

        ( velX, velY ) =
            minion.vel

        newAngle =
            atan2 (playerPosY - posY) (playerPosX - posX)

        ( accX, accY ) =
            fromPolar ( conf.minion.speed, newAngle )

        newVelX =
            (velX + accX) * conf.minion.friction

        newVelY =
            (velY + accY) * conf.minion.friction

        newPosX =
            posX + newVelX

        newPosY =
            posY + newVelY

        clampedPos =
            ( (clamp conf.minion.rad (arenaWidth - conf.minion.rad) newPosX)
            , (clamp conf.minion.rad (arenaHeight - conf.minion.rad) newPosY)
            )

        newVel =
            ( newVelX, newVelY )

        newMinion =
            { minion
                | pos = clampedPos
                , vel = newVel
            }
    in
        { minion
            | pos = ( newPosX, newPosY )
            , vel = newVel
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
            fromPolar ( conf.bullet.speed, bullet.angle )

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
