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

type alias Cell = ((Int,Int),Char)

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
    indicesInRow y
    |> L.filterMap (\ i2 -> get i2 puzzle)
    |> S.fromList

usedCharsInColumn : Int -> Puzzle -> S.Set Char
usedCharsInColumn x puzzle =
    indicesInColumn x
    |> L.filterMap (\ i2 -> get i2 puzzle)
    |> S.fromList

usedCharsInHouse : Int -> Puzzle -> S.Set Char
usedCharsInHouse h puzzle =
    indicesInHouse h
    |> L.filterMap (\ i2_ -> get i2_ puzzle)
    |> S.fromList

usedChars : (Int,Int) -> Puzzle -> S.Set Char
usedChars (x,y) puzzle =
    usedCharsInRow y puzzle
    |> S.union (usedCharsInColumn x puzzle)
    |> S.union (usedCharsInHouse (houseIndex (x,y)) puzzle)

availableChars : (Int,Int) -> Puzzle -> S.Set Char
availableChars i2 puzzle =
    case get i2 puzzle of
        Just _ -> S.empty
        Nothing -> S.diff puzzle.alphabet.filled (usedChars i2 puzzle)

indicesInRow : Int -> List (Int,Int)
indicesInRow y =
    L.range 0 8
    |> L.map (\ x -> (x,y))

indicesInColumn : Int -> List (Int,Int)
indicesInColumn x =
    L.range 0 8
    |> L.map (\ y -> (x,y))

indicesInHouse : Int -> List (Int,Int)
indicesInHouse h =
    L.repeat 9 (3 * (modBy 3 h), 3 * (h // 3))
    |> L.map2 (\ (x1,y1) (x2,y2) -> (x1 + x2, y1 + y2)) [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2),(2,0),(2,1),(2,2)]

houseIndex : (Int,Int) -> Int
houseIndex (x,y) = (3 * (y // 3)) + (x // 3)

unsolvedIndices : Puzzle -> List (Int,Int)
unsolvedIndices puzzle =
    let
        isMaybeEmpty : ((Int,Int), Maybe Char) -> Maybe (Int,Int)
        isMaybeEmpty (i2,mc) =
            case mc of
                Just _ -> Nothing
                Nothing -> Just i2
    in
        Mx.initialize (9,9) (\ i2 -> get i2 puzzle)
        |> Mx.toIndexedList
        |> L.filterMap isMaybeEmpty

-- Strategy

type Strategy
    = Direct

newlySolvedCells : Strategy -> Puzzle -> List Cell
newlySolvedCells strategy puzzle =
    case strategy of
        Direct ->
            let
                maybeNewlySolvedCell : (Int,Int) -> Maybe Cell
                maybeNewlySolvedCell i2 =
                    case availableChars i2 puzzle |> S.toList of
                        c :: [] -> Just (i2,c)
                        _ -> Nothing
            in
                unsolvedIndices puzzle
                |> L.filterMap maybeNewlySolvedCell

solve : List Strategy -> Puzzle -> Puzzle
solve strategies puzzle =
    let
        newlySolvedCells_ : List Cell
        newlySolvedCells_ = L.concatMap (\ strategy -> newlySolvedCells strategy puzzle) strategies
    in
        -- Tries the strategies in order, using the later strategies as little as possible.
        -- If the first fails, the it tries the second. If the second succeeds, then it tries the first again.
        case newlySolvedCells_ of
            [] -> puzzle
            (i2,c) :: _ -> solve strategies { puzzle | cells = D.insert i2 c puzzle.cells }
