module Tests exposing (..)

import Test exposing (..)
import Expect
import GameOfLife exposing (..)


all : Test
all =
    describe "Sample Test Suite"
        [ describe "Unit test examples"
            [ test "Addition" <|
                \() ->
                    Expect.equal (3 + 7) 10
            ]
        ]
