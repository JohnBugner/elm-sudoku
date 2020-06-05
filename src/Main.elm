module Main exposing (..)

import Browser as B
import Html as H
import Html.Attributes as HA
import Html.Events as HE

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
    { input = ""
    , output = ""
    }

type Event
    = GetInput String
    | CalcOutput

update : Event -> Model -> Model
update event model =
    case event of
        GetInput newInput -> { model | input = newInput }
        CalcOutput -> { model | output = String.reverse model.input }

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
