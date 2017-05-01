module Collisions exposing (checkCollisions)

-- mine

import Common exposing (..)


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
