module PuzzleOrError exposing (..)

import Dict as D
import List as L
import Matrix as Mx
import Result as R
import Set as S
import String as Str

import Puzzle

type alias PuzzleOrError = Result Error Puzzle.Puzzle

type Error
    = OutOfBoundsIndex ((Int,Int),Char)
    | IllegalCharacter ((Int,Int),Char)

fromString : Puzzle.Alphabet -> String -> PuzzleOrError
fromString alphabet string =
    Str.lines string
    |> L.map Str.toList
    |> fromList alphabet

fromList : Puzzle.Alphabet -> List (List Char) -> PuzzleOrError
fromList alphabet list =
    Mx.fromList list
    |> Mx.toIndexedList
    |> fromIndexedList alphabet

fromIndexedList : Puzzle.Alphabet -> List ((Int,Int),Char) -> PuzzleOrError
fromIndexedList alphabet cells =
    let
        maybeAddCell : ((Int,Int),Char) -> PuzzleOrError -> PuzzleOrError
        maybeAddCell cell result =
            let
                tryAddCell : ((Int,Int),Char) -> Puzzle.Puzzle -> PuzzleOrError
                tryAddCell ((x,y),c) puzzle =
                    if x <= 8 && y <= 8
                    then
                        if S.member c alphabet.filled || c == alphabet.empty
                        then Ok { puzzle | cells = D.insert (x,y) c puzzle.cells }
                        else Err (IllegalCharacter ((x,y),c))
                    else Err (OutOfBoundsIndex ((x,y),c))
            in
                R.andThen (tryAddCell cell) result
    in
        L.foldl maybeAddCell (Ok (Puzzle.empty alphabet)) cells

toString : PuzzleOrError -> String
toString puzzleOrError =
    case puzzleOrError of
        Ok puzzle -> Puzzle.toString puzzle
        Err error ->
            case error of
                OutOfBoundsIndex (i2,c) -> Str.concat ["Out of Bounds Index : ", Str.fromChar c, " at ", Debug.toString i2]
                IllegalCharacter (i2,c) -> Str.concat ["Illegal Character : ", Str.fromChar c, " at ", Debug.toString i2]
