module Error exposing (..)

import String as Str

import Puzzle

type Error
    = IndexIsOutOfBounds Puzzle.Cell
    | CharacterIsNotInAlphabet Puzzle.Cell
    | CharacterIsAlreadyInRow Puzzle.Cell
    | CharacterIsAlreadyInColumn Puzzle.Cell
    | CharacterIsAlreadyInHouse Puzzle.Cell

toString : Error -> String
toString error =
    let
        toString_ : String -> Puzzle.Cell -> String
        toString_ prefix (i2,c) = Str.concat [prefix, Str.fromChar c, " at ", Debug.toString i2]
    in
        case error of
            IndexIsOutOfBounds cell -> toString_ "Index is out of bounds : " cell
            CharacterIsNotInAlphabet cell -> toString_ "Character is not in alphabet : " cell
            CharacterIsAlreadyInRow cell -> toString_ "Character is already in row : " cell
            CharacterIsAlreadyInColumn cell -> toString_ "Character is already in column : " cell
            CharacterIsAlreadyInHouse cell -> toString_ "Character is already in house : " cell
