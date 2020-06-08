module Main exposing (..)

import Browser as B
import Html as H
import Html.Attributes as HA
import Html.Events as HE

import Result as R
import String as Str

import Puzzle
import PuzzleOrError

main =
    B.sandbox
        { init = init
        , update = update
        , view = view
        }

type alias Model =
    { input : String
    , output : String
    }

init : Model
init =
    { input =
        Str.join "\n"
            [ "2   8 3  "
            , " 6  7  84"
            , " 3 5  2 9"
            , "   1 54 8"
            , "         "
            , "4 27 6   "
            , "3 1  7 4 "
            , "72  4  6 "
            , "  4 1   3"
            ]
    , output = ""
    }

type Event
    = GetInput String
    | CalcOutput

update : Event -> Model -> Model
update event model =
    case event of
        GetInput newInput ->
            { model
            | input = newInput
            }
        CalcOutput ->
            { model
            | output =
                model.input
                |> PuzzleOrError.fromString Puzzle.numbersAlphabet
                |> R.map (Puzzle.solve [Puzzle.Direct])
                |> PuzzleOrError.toString
            }

view : Model -> H.Html Event
view model =
    H.div
        []
        [ H.textarea
            [ HA.placeholder "input"
            , HA.value model.input
            , HE.onInput GetInput
            ]
            []
        , H.input
            [ HA.type_ "button"
            , HA.value "Solve"
            , HE.onClick CalcOutput
            ]
            []
        , H.textarea
            [ HA.placeholder "output"
            , HA.readonly True
            , HA.value model.output
            ]
            []
        ]
