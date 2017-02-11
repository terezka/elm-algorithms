module BinarySearch exposing (..)

import Array


run : Int -> Array.Array Int -> Int
run searchValue listOfNumbers =
    binarySearch listOfNumbers searchValue 0 (Array.length listOfNumbers)


binarySearch : Array.Array Int -> Int -> Int -> Int -> Int
binarySearch listOfNumbers searchValue lo hi =
    if lo > hi then
        -1
    else
        let
            mid =
                lo + (hi - lo) // 2

            midValue =
                Array.get mid listOfNumbers
        in
            case midValue of
                Nothing ->
                    -1

                Just value ->
                    if searchValue < value then
                        binarySearch listOfNumbers searchValue lo (mid - 1)
                    else if searchValue > value then
                        binarySearch listOfNumbers searchValue (mid + 1) hi
                    else
                        mid
