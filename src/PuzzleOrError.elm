module PuzzleOrError exposing (..)

import Array as A
import Dict as D
import List as L
import Matrix as Mx
import Result as R
import Set as S
import String as Str

import Error
import Puzzle

type alias PuzzlesOrError = Result Error.Error (A.Array Puzzle.Puzzle)
type alias PuzzleOrError = Result Error.Error Puzzle.Puzzle

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
        addCell : Puzzle.Cell -> PuzzleOrError -> PuzzleOrError
        addCell cell result =
            let
                tryAddCell : Puzzle.Cell -> Puzzle.Puzzle -> PuzzleOrError
                tryAddCell ((x,y),c) puzzle =
                    if x >= 9 || y >= 9
                    then Err (Error.IndexIsOutOfBounds ((x,y),c))
                    else
                        if c == alphabet.empty
                        then Ok puzzle
                        else
                            if not <| S.member c alphabet.filled
                            then Err (Error.CharacterIsNotInAlphabet ((x,y),c))
                            else
                                if S.member c (Puzzle.usedCharsInRow y puzzle)
                                then Err (Error.CharacterIsAlreadyInRow ((x,y),c))
                                else
                                    if S.member c (Puzzle.usedCharsInColumn x puzzle)
                                    then Err (Error.CharacterIsAlreadyInColumn ((x,y),c))
                                    else
                                        if S.member c (Puzzle.usedCharsInHouse (Puzzle.houseIndex (x,y)) puzzle)
                                        then Err (Error.CharacterIsAlreadyInHouse ((x,y),c))
                                        else Ok { puzzle | grid = D.insert (x,y) c puzzle.grid }
            in
                R.andThen (tryAddCell cell) result
    in
        L.foldl addCell (Ok (Puzzle.empty alphabet)) cells

toStrings : PuzzlesOrError -> A.Array String
toStrings puzzlesOrError =
    case puzzlesOrError of
        Ok puzzles -> A.map Puzzle.toString puzzles
        Err error -> Error.toString error |> L.singleton |> A.fromList
