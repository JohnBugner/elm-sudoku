module Main exposing (..)

import Puzzle
import PuzzleOrError

import Browser
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode
import Json.Encode
import VirtualDom

import Array
import List
import Maybe
import Result
import String

main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }

type alias Model =
    { input : String
    , outputs : Array.Array String
    , outputIndex : Int
    }

init : Model
init =
    { input =
        String.join "\n"
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
    , outputs = Array.fromList []
    , outputIndex = -1
    }

type Event
    = GetInput String
    | CalcOutput
    | SelectOutputIndex Int

update : Event -> Model -> Model
update event model =
    case event of
        GetInput newInput ->
            { model
            | input = newInput
            }
        CalcOutput ->
            let
                outputs_ : Array.Array String
                outputs_ =
                    model.input
                    |> PuzzleOrError.fromString Puzzle.numbersAlphabet
                    |> Result.map (Puzzle.solve [Puzzle.Direct, Puzzle.Indirect])
                    |> PuzzleOrError.toStrings
            in
                { model
                | outputs = outputs_
                , outputIndex = Array.length outputs_ - 1
                }
        SelectOutputIndex index -> 
            { model
            | outputIndex = index
            }

view : Model -> H.Html Event
view model =
    let
        onInputTargetSelectedIndex : (Int -> msg) -> H.Attribute msg
        onInputTargetSelectedIndex tagger =
            let
                alwaysStop : a -> (a, Bool)
                alwaysStop x = (x, True)

                targetSelectedIndex : Json.Decode.Decoder Int
                targetSelectedIndex = Json.Decode.at ["target", "selectedIndex"] Json.Decode.int
            in
                HE.stopPropagationOn "input" (Json.Decode.map alwaysStop (Json.Decode.map tagger targetSelectedIndex))

        -- This doesn't work fully, but I don't know why...
        selectedIndex : Int -> H.Attribute msg
        selectedIndex =
            let
                intProperty : String -> Int -> H.Attribute msg
                intProperty key int = VirtualDom.property key (Json.Encode.int int)
            in
                intProperty "selectedIndex"

    in
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
            , H.select
                [ onInputTargetSelectedIndex SelectOutputIndex
                , selectedIndex model.outputIndex
                ]
                ( List.range 0 (Array.length model.outputs - 1)
                |> List.map (\ i -> H.option [] [H.text (String.fromInt i)])
                )
            , H.textarea
                [ HA.placeholder "output"
                , HA.readonly True
                , HA.value (Array.get model.outputIndex model.outputs |> Maybe.withDefault "")
                ]
                []
            ]
