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
    = IndexIsOutOfBounds Puzzle.Cell
    | CharacterIsNotInAlphabet Puzzle.Cell
    | CharacterIsAlreadyInRow Puzzle.Cell
    | CharacterIsAlreadyInColumn Puzzle.Cell
    | CharacterIsAlreadyInHouse Puzzle.Cell

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
                    then Err (IndexIsOutOfBounds ((x,y),c))
                    else
                        if c == alphabet.empty
                        then Ok puzzle
                        else
                            if not <| S.member c alphabet.filled
                            then Err (CharacterIsNotInAlphabet ((x,y),c))
                            else
                                if S.member c (Puzzle.usedCharsInRow y puzzle)
                                then Err (CharacterIsAlreadyInRow ((x,y),c))
                                else
                                    if S.member c (Puzzle.usedCharsInColumn x puzzle)
                                    then Err (CharacterIsAlreadyInColumn ((x,y),c))
                                    else
                                        if S.member c (Puzzle.usedCharsInHouse (x,y) puzzle)
                                        then Err (CharacterIsAlreadyInHouse ((x,y),c))
                                        else Ok { puzzle | cells = D.insert (x,y) c puzzle.cells }
            in
                R.andThen (tryAddCell cell) result
    in
        L.foldl addCell (Ok (Puzzle.empty alphabet)) cells

toString : PuzzleOrError -> String
toString puzzleOrError =
    case puzzleOrError of
        Ok puzzle -> Puzzle.toString puzzle
        Err error ->
            case error of
                IndexIsOutOfBounds (i2,c) ->
                    Str.concat ["Index is out of bounds : ", Str.fromChar c, " at ", Debug.toString i2]
                CharacterIsNotInAlphabet (i2,c) ->
                    Str.concat ["Character is not in alphabet : ", Str.fromChar c, " at ", Debug.toString i2]
                CharacterIsAlreadyInRow (i2,c) ->
                    Str.concat ["Character is already in row : ", Str.fromChar c, " at ", Debug.toString i2]
                CharacterIsAlreadyInColumn (i2,c) ->
                    Str.concat ["Character is already in column : ", Str.fromChar c, " at ", Debug.toString i2]
                CharacterIsAlreadyInHouse (i2,c) ->
                    Str.concat ["Character is already in house : ", Str.fromChar c, " at ", Debug.toString i2]
