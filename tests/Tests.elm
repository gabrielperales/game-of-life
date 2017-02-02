module Tests exposing (..)

import Test exposing (..)
import Expect
import GameOfLife exposing (..)
import Array
import List


all : Test
all =
    describe "Game of life"
        [ describe "empty"
            [ test "should create cells X * Y cells" <|
                \() ->
                    Expect.equal (empty ( 10, 10 ) |> .cells |> Array.length) 100
            , test "all the cells should be false" <|
                \() ->
                    Expect.true "All cells are false" (empty ( 10, 10 ) |> .cells |> Array.toList |> List.all ((==) False))
            , test "should store the board shape inside property shape" <|
                \() ->
                    Expect.equal (empty ( 10, 10 ) |> .shape) ( 10, 10 )
            ]
        , describe "isAlive"
            [ test "Alive cell will die if has less than 2 neighbors by underpopulation" <|
                \() ->
                    Expect.false "Cell died" (isAlive True 1)
            , test "Alive cell will survive if it has 2 heighbors" <|
                \() ->
                    Expect.true "Cell survive" (isAlive True 2)
            , test "Alive cell will survive if it has 3 heighbors" <|
                \() ->
                    Expect.true "Cell survive" (isAlive True 3)
            , test "Alive cell will die if it has more then 3 neighbors by overcrowding" <|
                \() ->
                    Expect.false "Cell died" (isAlive True 5)
            , test "A died cell will born if it has 3 neighbors" <|
                \() ->
                    Expect.true "Cell born" (isAlive False 3)
            ]
        ]
