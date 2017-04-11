module Input exposing (..)

import Common exposing (..)


updateKeyDown : Model -> Int -> Model
updateKeyDown model key =
    case dirFromKey key of
        Up ->
            { model | isMovingUp = True }

        Right ->
            { model | isMovingRight = True }

        Down ->
            { model | isMovingDown = True }

        Left ->
            { model | isMovingLeft = True }

        _ ->
            model


updateKeyUp : Model -> Int -> Model
updateKeyUp model key =
    case dirFromKey key of
        Up ->
            { model | isMovingUp = False }

        Right ->
            { model | isMovingRight = False }

        Down ->
            { model | isMovingDown = False }

        Left ->
            { model | isMovingLeft = False }

        _ ->
            model


dirFromKey : Int -> Dir
dirFromKey key =
    case key of
        87 ->
            Up

        68 ->
            Right

        83 ->
            Down

        65 ->
            Left

        _ ->
            None
