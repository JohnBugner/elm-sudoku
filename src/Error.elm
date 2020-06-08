module Error exposing (..)

import Puzzle

import String

type Error
    = IndexIsOutOfBounds Puzzle.FilledCell
    | CharacterIsNotInAlphabet Puzzle.FilledCell
    | CharacterIsAlreadyInRow Puzzle.FilledCell
    | CharacterIsAlreadyInColumn Puzzle.FilledCell
    | CharacterIsAlreadyInHouse Puzzle.FilledCell

toString : Error -> String
toString error =
    let
        toString_ : String -> Puzzle.FilledCell -> String
        toString_ prefix (i2,c) = String.concat [prefix, String.fromChar c, " at ", Debug.toString i2]
    in
        case error of
            IndexIsOutOfBounds cell -> toString_ "Index is out of bounds : " cell
            CharacterIsNotInAlphabet cell -> toString_ "Character is not in alphabet : " cell
            CharacterIsAlreadyInRow cell -> toString_ "Character is already in row : " cell
            CharacterIsAlreadyInColumn cell -> toString_ "Character is already in column : " cell
            CharacterIsAlreadyInHouse cell -> toString_ "Character is already in house : " cell
