module Matrix exposing (..)

import Array as A
import List as L
import Maybe as M

-- A matrix is always rectangular.
type alias Matrix a =
    { size : (Int,Int)
    , arrays : A.Array (A.Array a)
    }

-- Creation

empty : Matrix a
empty = Matrix (0,0) A.empty

initialize : (Int,Int) -> ((Int,Int) -> a) -> Matrix a
initialize (w,h) f =
    { size = (w,h)
    , arrays = A.initialize h (\ y -> A.initialize w (\ x -> f (x,y)))
    }

repeat : (Int,Int) -> a -> Matrix a
repeat (w,h) v =
    { size = (w,h)
    , arrays = A.repeat h (A.repeat w v)
    }

-- Takes the largest rectangle possible.
fromList : List (List a) -> Matrix a
fromList list =
    let
        w =
            L.map L.length list
            |> L.maximum
            |> M.withDefault 0
        h =
            if w == 0
            then 0
            else L.length list
    in
        { size = (w,h)
        , arrays =
            L.map A.fromList list
            |> L.map (A.slice 0 w)
            |> A.fromList
        }

-- Query

isEmpty : Matrix a -> Bool
isEmpty mx = mx.size == (0,0)

size : Matrix a -> (Int,Int)
size mx = mx.size

get : (Int,Int) -> Matrix a -> Maybe a
get (x,y) mx = A.get y mx.arrays |> M.andThen (A.get x)

-- Manipulate

set : (Int,Int) -> a -> Matrix a -> Matrix a 
set (x,y) v mx =
    case A.get y mx.arrays of
        Just row -> { mx | arrays = A.set y (A.set x v row) mx.arrays }
        Nothing -> mx

slice : (Int,Int) -> (Int,Int) -> Matrix a -> Matrix a
slice (xb,yb) (xe,ye) mx =
    let
        newArrays : A.Array (A.Array a)
        newArrays = A.slice yb ye mx.arrays |> A.map (A.slice xb xe)
    in
        { size =
            case A.get 0 newArrays of
                Just row -> (A.length row, A.length newArrays)
                Nothing -> (0, A.length newArrays)
        , arrays = newArrays
        }

-- Lists

toList : Matrix a -> List (List a)
toList mx = A.map A.toList mx.arrays |> A.toList

toIndexedList : Matrix a -> List ((Int,Int),a)
toIndexedList mx =
    toList mx
    |> L.indexedMap (\ y row -> L.indexedMap (\ x v -> ((x,y),v)) row)
    |> L.concat

-- Transform

map : (a -> b) -> Matrix a -> Matrix b
map f mx =
    { size = mx.size
    , arrays = A.map (\ row -> A.map f row) mx.arrays
    }

indexedMap : ((Int,Int) -> a -> b) -> Matrix a -> Matrix b
indexedMap f mx =
    { size = mx.size
    , arrays = A.indexedMap (\ y row -> A.indexedMap (\ x value -> f (x,y) value) row) mx.arrays
    }
