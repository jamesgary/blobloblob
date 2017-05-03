module Input exposing (updateKeyDown, updateKeyUp)

import Char
import Common exposing (..)


updateKeyDown : Model -> Int -> Model
updateKeyDown model key =
    let
        inputs =
            model.currentInputs

        newInputs =
            case inputFromKey key of
                Move Up ->
                    { inputs | isMovingUp = True }

                Move Right ->
                    { inputs | isMovingRight = True }

                Move Down ->
                    { inputs | isMovingDown = True }

                Move Left ->
                    { inputs | isMovingLeft = True }

                Fire Up ->
                    { inputs | isFiringUp = True }

                Fire Right ->
                    { inputs | isFiringRight = True }

                Fire Down ->
                    { inputs | isFiringDown = True }

                Fire Left ->
                    { inputs | isFiringLeft = True }

                Noop ->
                    inputs
    in
        { model | currentInputs = newInputs }


updateKeyUp : Model -> Int -> Model
updateKeyUp model key =
    let
        inputs =
            model.currentInputs

        newInputs =
            case inputFromKey key of
                Move Up ->
                    { inputs | isMovingUp = False }

                Move Right ->
                    { inputs | isMovingRight = False }

                Move Down ->
                    { inputs | isMovingDown = False }

                Move Left ->
                    { inputs | isMovingLeft = False }

                Fire Up ->
                    { inputs | isFiringUp = False }

                Fire Right ->
                    { inputs | isFiringRight = False }

                Fire Down ->
                    { inputs | isFiringDown = False }

                Fire Left ->
                    { inputs | isFiringLeft = False }

                Noop ->
                    inputs
    in
        { model | currentInputs = newInputs }


inputFromKey : Int -> Input
inputFromKey key =
    case Char.fromCode key of
        'W' ->
            Move Up

        'D' ->
            Move Right

        'S' ->
            Move Down

        'A' ->
            Move Left

        'I' ->
            Fire Up

        'L' ->
            Fire Right

        'K' ->
            Fire Down

        'J' ->
            Fire Left

        _ ->
            Noop
