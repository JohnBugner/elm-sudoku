module Puzzle exposing (..)

import Array
import Dict
import List
import Maybe
import Matrix
import Result
import Set
import String

type alias Puzzle =
    { alphabet : Alphabet
    , grid : Grid
    }

type alias Alphabet =
    { filled : Set.Set Char
    , empty : Char
    }

type alias Grid = Dict.Dict (Int,Int) Char
type alias FilledCell = ((Int,Int),Char)

-- Creation

empty : Alphabet -> Puzzle
empty alphabet =
    { alphabet = alphabet
    , grid = Dict.empty
    }

numbersAlphabet : Alphabet
numbersAlphabet =
    { filled = String.toList "123456789" |> Set.fromList
    , empty = ' '
    }

-- Lists

toString : Puzzle -> String
toString puzzle =
    toList puzzle
    |> List.map String.fromList
    |> String.join "\n"

toList : Puzzle -> List (List Char)
toList puzzle =
    Matrix.initialize (9,9) (\ i2 -> get i2 puzzle |> Maybe.withDefault puzzle.alphabet.empty)
    |> Matrix.toList

-- Query

get : (Int,Int) -> Puzzle -> Maybe Char
get i2 puzzle = Dict.get i2 puzzle.grid

usedCharsInRow : Int -> Puzzle -> Set.Set Char
usedCharsInRow y puzzle =
    indicesInRow y
    |> List.filterMap (\ i2 -> get i2 puzzle)
    |> Set.fromList

usedCharsInColumn : Int -> Puzzle -> Set.Set Char
usedCharsInColumn x puzzle =
    indicesInColumn x
    |> List.filterMap (\ i2 -> get i2 puzzle)
    |> Set.fromList

usedCharsInHouse : Int -> Puzzle -> Set.Set Char
usedCharsInHouse h puzzle =
    indicesInHouse h
    |> List.filterMap (\ i2_ -> get i2_ puzzle)
    |> Set.fromList

usedChars : (Int,Int) -> Puzzle -> Set.Set Char
usedChars (x,y) puzzle =
    usedCharsInRow y puzzle
    |> Set.union (usedCharsInColumn x puzzle)
    |> Set.union (usedCharsInHouse (houseIndex (x,y)) puzzle)

availableChars : (Int,Int) -> Puzzle -> Set.Set Char
availableChars i2 puzzle =
    case get i2 puzzle of
        Just _ -> Set.empty
        Nothing -> Set.diff puzzle.alphabet.filled (usedChars i2 puzzle)

indicesInRow : Int -> List (Int,Int)
indicesInRow y =
    List.range 0 8
    |> List.map (\ x -> (x,y))

indicesInColumn : Int -> List (Int,Int)
indicesInColumn x =
    List.range 0 8
    |> List.map (\ y -> (x,y))

indicesInHouse : Int -> List (Int,Int)
indicesInHouse h =
    List.repeat 9 (3 * (modBy 3 h), 3 * (h // 3))
    |> List.map2 (\ (x1,y1) (x2,y2) -> (x1 + x2, y1 + y2)) [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2),(2,0),(2,1),(2,2)]

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
        Matrix.initialize (9,9) (\ i2 -> get i2 puzzle)
        |> Matrix.toIndexedList
        |> List.filterMap isMaybeEmpty

-- Strategy

type Strategy
    = Direct

newlySolvedCells : Strategy -> Puzzle -> List FilledCell
newlySolvedCells strategy puzzle =
    case strategy of
        Direct ->
            let
                maybeNewlySolvedCell : (Int,Int) -> Maybe FilledCell
                maybeNewlySolvedCell i2 =
                    case availableChars i2 puzzle |> Set.toList of
                        c :: [] -> Just (i2,c)
                        _ -> Nothing
            in
                unsolvedIndices puzzle
                |> List.filterMap maybeNewlySolvedCell

solve : List Strategy -> Puzzle -> Array.Array Puzzle
solve strategies puzzle =
    let
        solve_ : List Puzzle -> Puzzle -> List Puzzle
        solve_ puzzles latestPuzzle =
            let
                newlySolvedCells_ : List FilledCell
                newlySolvedCells_ = List.concatMap (\ strategy -> newlySolvedCells strategy latestPuzzle) strategies

                newPuzzles : List Puzzle
                newPuzzles = latestPuzzle :: puzzles
            in
                -- Tries the strategies in order, using the later strategies as little as possible.
                -- If the first fails, the it tries the second. If the second succeeds, then it tries the first again.
                case newlySolvedCells_ of
                    [] -> newPuzzles
                    (i2,c) :: _ -> solve_ newPuzzles { latestPuzzle | grid = Dict.insert i2 c latestPuzzle.grid }
    in
        solve_ [] puzzle
        |> List.reverse
        |> Array.fromList
