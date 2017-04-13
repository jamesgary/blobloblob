module Input exposing (..)

import Common exposing (..)


updateKeyDown : Model -> Int -> Model
updateKeyDown model key =
    case inputFromKey key of
        MoveUp ->
            { model | isMovingUp = True }

        MoveRight ->
            { model | isMovingRight = True }

        MoveDown ->
            { model | isMovingDown = True }

        MoveLeft ->
            { model | isMovingLeft = True }

        Fire ->
            { model | isFiring = True }

        _ ->
            model


updateKeyUp : Model -> Int -> Model
updateKeyUp model key =
    case inputFromKey key of
        MoveUp ->
            { model | isMovingUp = False }

        MoveRight ->
            { model | isMovingRight = False }

        MoveDown ->
            { model | isMovingDown = False }

        MoveLeft ->
            { model | isMovingLeft = False }

        Fire ->
            { model | isFiring = False }

        _ ->
            model



-- http://keycode.info/


inputFromKey : Int -> Input
inputFromKey key =
    case key of
        87 ->
            MoveUp

        68 ->
            MoveRight

        83 ->
            MoveDown

        65 ->
            MoveLeft

        74 ->
            Fire

        _ ->
            None
