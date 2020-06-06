module Main exposing (..)

import Browser as B
import Html as H
import Html.Attributes as HA
import Html.Events as HE

import String as Str

import Puzzle

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
        [ "  3 2 6  "
        , "9  3 5  1"
        , "  18 64  "
        , "  81 29  "
        , "7       8"
        , "  67 82  "
        , "  26 95  "
        , "8  2 3  9"
        , "  5 1 3  "
        , ""
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
                |> Puzzle.fromString Puzzle.numbersAlphabet
                |> Puzzle.solve []
                |> Puzzle.toString
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
