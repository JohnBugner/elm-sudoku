module Main exposing (..)

import Html as H
import Html.Attributes as HA

main =
    H.div []
        [ H.textarea
            [ HA.placeholder "input"
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
            ]
            []
        ]
