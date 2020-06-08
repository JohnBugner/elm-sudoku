module Main exposing (..)

import Browser as B
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD
import Json.Encode as JE
import VirtualDom

import Array as A
import List as L
import Maybe as M
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
    , outputs : A.Array String
    , outputIndex : Int
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
    , outputs = A.fromList []
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
                outputs_ : A.Array String
                outputs_ =
                    model.input
                    |> PuzzleOrError.fromString Puzzle.numbersAlphabet
                    |> R.map (Puzzle.solve [Puzzle.Direct])
                    |> PuzzleOrError.toStrings
            in
                { model
                | outputs = outputs_
                , outputIndex = A.length outputs_ - 1
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

                targetSelectedIndex : JD.Decoder Int
                targetSelectedIndex = JD.at ["target", "selectedIndex"] JD.int
            in
                HE.stopPropagationOn "input" (JD.map alwaysStop (JD.map tagger targetSelectedIndex))

        -- This doesn't work fully, but I don't know why...
        selectedIndex : Int -> H.Attribute msg
        selectedIndex =
            let
                intProperty : String -> Int -> H.Attribute msg
                intProperty key int = VirtualDom.property key (JE.int int)
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
                ( L.range 0 (A.length model.outputs - 1)
                |> L.map (\ i -> H.option [] [H.text (Str.fromInt i)])
                )
            , H.textarea
                [ HA.placeholder "output"
                , HA.readonly True
                , HA.value (A.get model.outputIndex model.outputs |> M.withDefault "")
                ]
                []
            ]
