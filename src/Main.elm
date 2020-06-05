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
    }

init : Model
init =
    { input = ""
    }

type Event
    = Solve String

update : Event -> Model -> Model
update event model =
    case event of
        Solve newInput -> { model | input = newInput }

view : Model -> H.Html Event
view model =
    H.div
        []
        [ H.textarea
            [ HA.placeholder "input"
            , HA.value model.input
            , HE.onInput Solve
            ]
            []
        , H.input
            [ HA.type_ "button"
            , HA.value "Solve"
            ]
            []
        , H.textarea
            [ HA.placeholder "output"
            , HA.readonly True
            , HA.value <| String.reverse model.input
            ]
            []
        ]
