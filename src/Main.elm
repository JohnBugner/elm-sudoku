module Main exposing (..)

import Browser as B
import Html as H
import Html.Attributes as HA
import Html.Events as HE

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
    { input = "003020600\n900305001\n001806400\n008102900\n700000008\n006708200\n002609500\n800203009\n005010300\n"
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
