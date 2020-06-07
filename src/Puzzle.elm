module Puzzle exposing (..)

import Dict as D
import List as L
import Maybe as M
import Matrix as Mx
import Result as R
import Set as S
import String as Str

type alias Puzzle =
    { alphabet : Alphabet
    , cells : D.Dict (Int,Int) Char
    }

type alias Alphabet =
    { filled : S.Set Char
    , empty : Char
    }

-- Creation

empty : Alphabet -> Puzzle
empty alphabet =
    { alphabet = alphabet
    , cells = D.empty
    }

numbersAlphabet : Alphabet
numbersAlphabet =
    { filled = Str.toList "123456789" |> S.fromList
    , empty = ' '
    }

-- Lists

toString : Puzzle -> String
toString puzzle =
    toList puzzle
    |> L.map Str.fromList
    |> Str.join "\n"

toList : Puzzle -> List (List Char)
toList puzzle =
    Mx.initialize (9,9) (\ i2 -> get i2 puzzle |> M.withDefault puzzle.alphabet.empty)
    |> Mx.toList

-- Query

get : (Int,Int) -> Puzzle -> Maybe Char
get i2 puzzle = D.get i2 puzzle.cells

-- Strategy

-- fix
type Strategy
    = None

-- fix
solve : List Strategy -> Puzzle -> Puzzle
solve strategies puzzle = puzzle
