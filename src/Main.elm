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
import Input exposing (..)
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
            { pos = ( 400, 225 )
            , vel = ( 0, 0 )
            , health = 1000
            , rad = conf.player.rad
            }
      , arenaSize = ( 800, 450 )
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
            [ { pos = ( 50, 50 ), health = conf.spawn.maxHealth, rad = conf.spawn.rad }
            , { pos = ( 150, 50 ), health = conf.spawn.maxHealth, rad = conf.spawn.rad }
            , { pos = ( 350, 50 ), health = conf.spawn.maxHealth, rad = conf.spawn.rad }
            , { pos = ( 550, 50 ), health = conf.spawn.maxHealth, rad = conf.spawn.rad }
            , { pos = ( 750, 50 ), health = conf.spawn.maxHealth, rad = conf.spawn.rad }
            , { pos = ( 50, 400 ), health = conf.spawn.maxHealth, rad = conf.spawn.rad }
            , { pos = ( 750, 400 ), health = conf.spawn.maxHealth, rad = conf.spawn.rad }
            ]
      , minions =
            [ { pos = ( 300, 200 )
              , vel = ( 1, 2 )
              , rad = conf.minion.rad
              , health = conf.minion.maxHealth
              }
            ]
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
    , vel = ( 1, 2 )
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


checkCollisions : Model -> Model
checkCollisions model =
    let
        ( spawns, bullets ) =
            List.foldr collideObjWithObjs ( [], model.bullets ) model.spawns

        ( minions, bullets2, effects ) =
            collideMinionsWithBullets model.minions bullets

        ( player, minions2, effects2 ) =
            collidePlayerWithMinions model.player minions

        combinedEffects =
            List.append effects effects2
    in
        { model
            | spawns = spawns
            , minions = minions2
            , bullets = bullets2
            , player = player
            , effects = List.append model.effects combinedEffects
        }


collideMinionsWithBullets : List Minion -> List Bullet -> ( List Minion, List Bullet, List Effect )
collideMinionsWithBullets minions bullets =
    case minions of
        minion :: otherMinions ->
            let
                ( maybeMinion, unhitBullets, effects ) =
                    collideMinionWithBullets minion bullets

                ( newMinions, unhitBullets2, effects2 ) =
                    collideMinionsWithBullets otherMinions unhitBullets
            in
                case maybeMinion of
                    Just damagedMinion ->
                        ( damagedMinion :: newMinions, unhitBullets2, List.append effects effects2 )

                    Nothing ->
                        ( newMinions, unhitBullets2, List.append effects effects2 )

        [] ->
            ( [], bullets, [] )


collideMinionWithBullets : Minion -> List Bullet -> ( Maybe Minion, List Bullet, List Effect )
collideMinionWithBullets minion bullets =
    case bullets of
        bullet :: otherBullets ->
            if collideObjects minion bullet then
                let
                    damagedMinion =
                        { minion | health = minion.health - conf.bullet.dmg }

                    effect =
                        effectFromCollidable bullet
                in
                    if damagedMinion.health > 0.0 then
                        let
                            ( maybeMinion, unhitBullets, effects ) =
                                collideMinionWithBullets damagedMinion otherBullets
                        in
                            ( maybeMinion, unhitBullets, effect :: effects )
                    else
                        -- he's dead, jim
                        ( Nothing, otherBullets, [ effect ] )
            else
                let
                    ( maybeMinion, unhitBullets, effects ) =
                        collideMinionWithBullets minion otherBullets
                in
                    ( maybeMinion, bullet :: unhitBullets, effects )

        [] ->
            ( Just minion, [], [] )


effectFromCollidable : Collidable a -> Effect
effectFromCollidable collidable =
    { pos = collidable.pos
    , age = 0
    }


collidePlayerWithMinions : Player -> List Minion -> ( Player, List Minion, List Effect )
collidePlayerWithMinions player minions =
    let
        ( collidedMinions, uncollidedMinions ) =
            List.partition (collideObjects player) minions

        amtDmg =
            100.0 * toFloat (List.length collidedMinions)
    in
        ( { player | health = player.health - amtDmg }
        , uncollidedMinions
        , List.map effectFromCollidable collidedMinions
        )


collideObjWithObjs :
    Collidable a
    -> ( List (Collidable a), List (Collidable b) )
    -> ( List (Collidable a), List (Collidable b) )
collideObjWithObjs obj1 ( obj2s, obj1s ) =
    case obj1s of
        obj :: otherObjs ->
            if collideObjects obj1 obj then
                ( { obj1 | health = obj1.health - conf.bullet.dmg } :: obj2s, otherObjs )
            else
                let
                    ( newSpawns, newBullets ) =
                        collideObjWithObjs obj1 ( obj2s, otherObjs )
                in
                    ( newSpawns, obj :: newBullets )

        [] ->
            ( obj1 :: obj2s, obj1s )


collideObjects : Collidable a -> Collidable b -> Bool
collideObjects obj1 obj2 =
    -- is a^2 + b^2 > (rad1 + rad2)^2?
    let
        ( x1, y1 ) =
            obj1.pos

        ( x2, y2 ) =
            obj2.pos

        a =
            x1 - x2

        b =
            y1 - y2

        c =
            obj1.rad + obj2.rad
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


moveMinions : Time.Time -> Model -> Model
moveMinions time model =
    { model
        | minions = List.map (moveMinion time model.arenaSize) model.minions
    }


moveMinion : Time.Time -> ( Float, Float ) -> Minion -> Minion
moveMinion time ( arenaWidth, arenaHeight ) minion =
    let
        ( xDelta, yDelta ) =
            minion.vel

        ( x, y ) =
            minion.pos

        newX =
            x + xDelta

        newY =
            y + yDelta

        newXDelta =
            if (newX < 0 || newX > arenaWidth) then
                -1 * xDelta
            else
                xDelta

        newYDelta =
            if (newY < 0 || newY > arenaHeight) then
                -1 * yDelta
            else
                yDelta

        newX2 =
            x + newXDelta

        newY2 =
            y + newYDelta
    in
        { minion
            | pos = ( newX2, newY2 )
            , vel = ( newXDelta, newYDelta )
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
