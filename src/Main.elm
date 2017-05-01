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
        ( spawns, bullets, effects ) =
            collideObjsWithObjsWithEffects model.spawns model.bullets collideSpawnAndBullet

        ( minions, bullets2, effects2 ) =
            collideObjsWithObjsWithEffects model.minions bullets collideMinionAndBullet

        ( player, minions2, effects3 ) =
            collideObjWithObjsWithEffects model.player minions collidePlayerAndMinion

        playerToReturn =
            case player of
                Just p ->
                    p

                Nothing ->
                    -- should never happen, but return orig player just in case
                    model.player

        combinedEffects =
            List.append effects (List.append effects2 effects3)
    in
        { model
            | spawns = spawns
            , minions = minions2
            , bullets = bullets2
            , player = playerToReturn
            , effects = List.append model.effects combinedEffects
        }


collideSpawnAndBullet : Spawn -> Bullet -> ( Maybe Spawn, Maybe Bullet, List Effect )
collideSpawnAndBullet spawn bullet =
    if collideObjects spawn bullet then
        let
            damagedSpawn =
                { spawn | health = spawn.health - conf.bullet.dmg }

            effect =
                effectFromCollidable BulletHit bullet
        in
            if damagedSpawn.health > 0.0 then
                ( Just damagedSpawn, Nothing, [ effect ] )
            else
                ( Nothing, Nothing, [ effect, effectFromCollidable SpawnDeath damagedSpawn ] )
    else
        ( Just spawn, Just bullet, [] )


collideMinionAndBullet : Minion -> Bullet -> ( Maybe Minion, Maybe Bullet, List Effect )
collideMinionAndBullet minion bullet =
    if collideObjects minion bullet then
        let
            damagedMinion =
                { minion | health = minion.health - conf.bullet.dmg }

            effect =
                effectFromCollidable BulletHit bullet
        in
            if damagedMinion.health > 0.0 then
                ( Just damagedMinion, Nothing, [ effect ] )
            else
                ( Nothing, Nothing, [ effect, effectFromCollidable MinionDeath damagedMinion ] )
    else
        ( Just minion, Just bullet, [] )


collidePlayerAndMinion : Player -> Minion -> ( Maybe Player, Maybe Minion, List Effect )
collidePlayerAndMinion player minion =
    if collideObjects player minion then
        let
            damagedPlayer =
                { player | health = player.health - conf.minion.dmg }

            effect =
                effectFromCollidable MinionDeath minion
        in
            ( Just damagedPlayer, Nothing, [ effect ] )
    else
        ( Just player, Just minion, [] )


collideObjsWithObjsWithEffects : List a -> List b -> (a -> b -> ( Maybe a, Maybe b, List Effect )) -> ( List a, List b, List Effect )
collideObjsWithObjsWithEffects objAs objBs collideFunc =
    case objAs of
        objA :: otherObjAs ->
            let
                ( maybeObjA, objBs2, effects ) =
                    collideObjWithObjsWithEffects objA objBs collideFunc

                ( newObjAs, newObjBs, effects2 ) =
                    collideObjsWithObjsWithEffects otherObjAs objBs2 collideFunc

                objAsToReturn =
                    case maybeObjA of
                        Just justObjA ->
                            (justObjA :: newObjAs)

                        Nothing ->
                            (newObjAs)
            in
                ( objAsToReturn, newObjBs, List.append effects effects2 )

        [] ->
            ( [], objBs, [] )


collideObjWithObjsWithEffects : a -> List b -> (a -> b -> ( Maybe a, Maybe b, List Effect )) -> ( Maybe a, List b, List Effect )
collideObjWithObjsWithEffects objA objBs collideFunc =
    case objBs of
        objB :: otherObjBs ->
            let
                ( maybeA, maybeB, effects ) =
                    collideFunc objA objB
            in
                case maybeA of
                    Just justObjA ->
                        let
                            ( newMaybeA, newObjBs, newEffects ) =
                                collideObjWithObjsWithEffects justObjA otherObjBs collideFunc

                            objBsToReturn =
                                case maybeB of
                                    Just justObjB ->
                                        justObjB :: newObjBs

                                    Nothing ->
                                        newObjBs
                        in
                            ( newMaybeA, objBsToReturn, List.append effects newEffects )

                    Nothing ->
                        let
                            objBsToReturn =
                                case maybeB of
                                    Just justObjB ->
                                        justObjB :: otherObjBs

                                    Nothing ->
                                        otherObjBs
                        in
                            ( Nothing, objBsToReturn, effects )

        [] ->
            ( Just objA, objBs, [] )


effectFromCollidable : EffectType -> Collidable a -> Effect
effectFromCollidable effectType collidable =
    { pos = collidable.pos
    , age = 0
    , type_ = effectType
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
        , List.map (effectFromCollidable MinionDeath) collidedMinions
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
