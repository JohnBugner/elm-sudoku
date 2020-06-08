module PuzzleOrError exposing (..)

import Error
import Puzzle

import Array
import Dict
import List
import Matrix
import Result
import Set
import String

type alias PuzzleOrError = Result Error.Error Puzzle.Puzzle
type alias PuzzlesOrError = Result Error.Error (Array.Array Puzzle.Puzzle)

fromString : Puzzle.Alphabet -> String -> PuzzleOrError
fromString alphabet string =
    String.lines string
    |> List.map String.toList
    |> fromList alphabet

fromList : Puzzle.Alphabet -> List (List Char) -> PuzzleOrError
fromList alphabet list =
    Matrix.fromList list
    |> Matrix.toIndexedList
    |> fromIndexedList alphabet

fromIndexedList : Puzzle.Alphabet -> List ((Int,Int),Char) -> PuzzleOrError
fromIndexedList alphabet cells =
    let
        addCell : Puzzle.FilledCell -> PuzzleOrError -> PuzzleOrError
        addCell cell result =
            let
                tryAddCell : Puzzle.FilledCell -> Puzzle.Puzzle -> PuzzleOrError
                tryAddCell ((x,y),c) puzzle =
                    if x >= 9 || y >= 9
                    then Err (Error.IndexIsOutOfBounds ((x,y),c))
                    else
                        if c == alphabet.empty
                        then Ok puzzle
                        else
                            if not <| Set.member c alphabet.filled
                            then Err (Error.CharacterIsNotInAlphabet ((x,y),c))
                            else
                                if Set.member c (Puzzle.usedCharsInRow y puzzle)
                                then Err (Error.CharacterIsAlreadyInRow ((x,y),c))
                                else
                                    if Set.member c (Puzzle.usedCharsInColumn x puzzle)
                                    then Err (Error.CharacterIsAlreadyInColumn ((x,y),c))
                                    else
                                        if Set.member c (Puzzle.usedCharsInHouse (Puzzle.houseIndex (x,y)) puzzle)
                                        then Err (Error.CharacterIsAlreadyInHouse ((x,y),c))
                                        else Ok { puzzle | grid = Dict.insert (x,y) c puzzle.grid }
            in
                Result.andThen (tryAddCell cell) result
    in
        List.foldl addCell (Ok (Puzzle.empty alphabet)) cells

toStrings : PuzzlesOrError -> Array.Array String
toStrings puzzlesOrError =
    case puzzlesOrError of
        Ok puzzles -> Array.map Puzzle.toString puzzles
        Err error -> Error.toString error |> List.singleton |> Array.fromList
