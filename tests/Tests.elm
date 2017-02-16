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
                    Expect.equal 100 (empty ( 10, 10 ) |> .cells |> Array.length)
            , test "all the cells should died" <|
                \() ->
                    Expect.true "All cells are false (died)" (empty ( 10, 10 ) |> .cells |> Array.toList |> List.all ((==) False))
            , test "should store the board shape inside property shape" <|
                \() ->
                    Expect.equal ( 10, 10 ) (empty ( 10, 10 ) |> .shape)
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
        , describe "indexToCoords" <|
            [ test "index 0 should always be the coords (0, 0)" <|
                \() ->
                    Expect.equal ( 0, 0 ) (indexToCoords ( 3, 3 ) 0)
            , test "index 4 in a shape (3, 3) should  be the coords (1, 1)" <|
                \() ->
                    Expect.equal ( 1, 1 ) (indexToCoords ( 3, 3 ) 4)
            , test "index 8 in a shape (3, 3) should  be the coords (2, 2)" <|
                \() ->
                    Expect.equal ( 2, 2 ) (indexToCoords ( 3, 3 ) 8)
            , test "index 7 in a shape (5, 5) should  be the coords (1, 2)" <|
                \() ->
                    Expect.equal ( 1, 2 ) (indexToCoords ( 5, 5 ) 7)
            , test "index 7 in a shape (5, 5) should  be the coords (3, 2)" <|
                \() ->
                    Expect.equal ( 3, 2 ) (indexToCoords ( 5, 5 ) 17)
            ]
        , describe "coordsToIndex" <|
            [ test "coords (0, 0) should always be index 0" <|
                \() ->
                    Expect.equal 0 (coordsToIndex ( 10, 10 ) ( 0, 0 ))
            , test "coords (2, 2) in a shape (3, 3) should be index 8" <|
                \() ->
                    Expect.equal 8 (coordsToIndex ( 3, 3 ) ( 2, 2 ))
            , test "coords (1, 1) in a shape (3, 3) should be index 4" <|
                \() ->
                    Expect.equal 4 (coordsToIndex ( 3, 3 ) ( 1, 1 ))
            , test "coords (0, -1) in a shape (3, 3) should be index 2" <|
                \() ->
                    Expect.equal 2 (coordsToIndex ( 3, 3 ) ( 0, -1 ))
            , test "coords (-1, 0) in a shape (3, 3) should be index 6" <|
                \() ->
                    Expect.equal 6 (coordsToIndex ( 3, 3 ) ( -1, 0 ))
            , test "coords (2, -1) in a shape (3, 3) should be index 8" <|
                \() ->
                    Expect.equal 8 (coordsToIndex ( 3, 3 ) ( 2, -1 ))
            , test "coords (-1, 2) in a shape (3, 3) should be index 8" <|
                \() ->
                    Expect.equal 8 (coordsToIndex ( 3, 3 ) ( -1, 2 ))
            , test "coords (-1, -1) in a shape (3, 3) should be index 8" <|
                \() ->
                    Expect.equal 8 (coordsToIndex ( 3, 3 ) ( -1, -1 ))
            ]
        , describe "neightbors" <|
            [ test "It should always return 8 coordinates" <|
                \() ->
                    Expect.equal 8 (List.length <| neighbors ( 0, 0 ))
            , test "Neighbors of (0, 0)" <|
                \() ->
                    Expect.equal [ ( -1, -1 ), ( -1, 0 ), ( -1, 1 ), ( 0, -1 ), ( 0, 1 ), ( 1, -1 ), ( 1, 0 ), ( 1, 1 ) ] (neighbors ( 0, 0 ))
            ]
        , describe "neightborsAlive" <|
            [ test "Alive cell in a empty board" <|
                \() ->
                    let
                        board =
                            Array.fromList []

                        indexes =
                            [ 1, 2, 3, 4 ]
                    in
                        Expect.equal 0 (neighborsAlive board indexes)
            , test "Alive cell in a board with all cells died" <|
                \() ->
                    let
                        board =
                            Array.fromList [ False, False, False, False, False ]

                        indexes =
                            [ 1, 2, 3, 4 ]
                    in
                        Expect.equal 0 (neighborsAlive board indexes)
            , test "It should count all True values in a Array" <|
                \() ->
                    let
                        board =
                            Array.fromList [ True, False, True, True ]

                        indexes =
                            -- Indexes -,1 , 4 and 5 are not in the array
                            [ 0, 1, 2, 3, -1, 4, 5 ]
                    in
                        Expect.equal 3 (neighborsAlive board indexes)
            ]
        , describe "neighborsIndexes" <|
            [ test "In order coords shape (2, 2)" <|
                \() ->
                    let
                        boardShape =
                            ( 2, 2 )

                        cellCoords =
                            [ ( 0, 0 ), ( 0, 1 ), ( 1, 0 ), ( 1, 1 ) ]
                    in
                        Expect.equal [ 0, 1, 2, 3 ] (neighborsIndexes boardShape cellCoords)
            , test "Negative coords shape (2, 2)" <|
                \() ->
                    let
                        boardShape =
                            ( 2, 2 )

                        cellCoords =
                            [ ( 0, 0 ), ( 0, -1 ), ( -1, 0 ), ( -1, -1 ) ]
                    in
                        Expect.equal [ 0, 1, 2, 3 ] (neighborsIndexes boardShape cellCoords)
            , test "Negative and positive coords shape (3, 3)" <|
                \() ->
                    let
                        boardShape =
                            ( 3, 3 )

                        cellCoords =
                            [ ( 0, 0 ), ( 0, -1 ), ( -1, 0 ), ( -1, -1 ), ( 1, 1 ), ( 2, 2 ) ]
                    in
                        Expect.equal [ 0, 2, 6, 8, 4, 8 ] (neighborsIndexes boardShape cellCoords)
            ]
        , describe "lifeCycle" <|
            [ test "Blinker" <|
                \() ->
                    let
                        initialCells =
                            Array.fromList
                                [ False
                                , False
                                , False
                                , False
                                , False
                                , False
                                , False
                                , False
                                , False
                                , False
                                , False
                                , True
                                , True
                                , True
                                , False
                                , False
                                , False
                                , False
                                , False
                                , False
                                , False
                                , False
                                , False
                                , False
                                , False
                                ]

                        initialModel =
                            { shape = ( 5, 5 )
                            , cells = initialCells
                            , paused = False
                            }

                        expectedCells =
                            Array.fromList
                                [ False
                                , False
                                , False
                                , False
                                , False
                                , False
                                , False
                                , True
                                , False
                                , False
                                , False
                                , False
                                , True
                                , False
                                , False
                                , False
                                , False
                                , True
                                , False
                                , False
                                , False
                                , False
                                , False
                                , False
                                , False
                                ]
                    in
                        Expect.equal expectedCells (.cells (lifeCycle initialModel))
            ]
        ]



--ortinen@gmail.com
--GlB10AbC
