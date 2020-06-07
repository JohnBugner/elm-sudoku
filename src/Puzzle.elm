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

usedCharsInRow : Int -> Puzzle -> S.Set Char
usedCharsInRow y puzzle =
    L.range 0 8
    |> L.filterMap (\ x -> get (x,y) puzzle)
    |> S.fromList

usedCharsInColumn : Int -> Puzzle -> S.Set Char
usedCharsInColumn x puzzle =
    L.range 0 8
    |> L.filterMap (\ y -> get (x,y) puzzle)
    |> S.fromList

usedCharsInHouse : (Int,Int) -> Puzzle -> S.Set Char
usedCharsInHouse i2 puzzle =
    indicesInHouse (houseIndex i2)
    |> L.filterMap (\ i2_ -> get i2_ puzzle)
    |> S.fromList

houseIndex : (Int,Int) -> Int
houseIndex (x,y) = (3 * (y // 3)) + (x // 3)

-- fix
indicesInHouse : Int -> List (Int,Int)
indicesInHouse h =
    L.repeat 9 (3 * (modBy 3 h), 3 * (h // 3))
    |> L.map2 (\ (x1,y1) (x2,y2) -> (x1 + x2, y1 + y2)) [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2),(2,0),(2,1),(2,2)]

-- Strategy

-- fix
type Strategy
    = None

-- fix
solve : List Strategy -> Puzzle -> Puzzle
solve strategies puzzle = puzzle
