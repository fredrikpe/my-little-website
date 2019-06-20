module UtilTest exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode as Decode
import Test exposing (..)
import Util


suite : Test
suite =
    describe "The Util module"
        [ describe "List operations"
            [ fuzz (list int) "(Slice 0 1) == (take 1)" <|
                \fuzzList ->
                    let
                        copy =
                            fuzzList
                    in
                    Expect.equalLists (Util.slice 0 1 fuzzList) (List.take 1 copy)
            , test "slice 1" <|
                \_ ->
                    Expect.equalLists (Util.slice 2 5 [ 0, 1, 2, 3, 4, 5 ]) [ 2, 3, 4 ]
            , test "slice 2" <|
                \_ ->
                    Expect.equalLists (Util.slice 5 2 [ 0, 1, 2, 3, 4, 5 ]) []
            , test "indexOf 1" <|
                \_ ->
                    Expect.equal (Util.indexOf 5 [ 0, 1, 2, 3, 4, 5 ]) (Just 5)
            , test "indexOf 2" <|
                \_ ->
                    Expect.equal (Util.indexOf 56 [ 0, 1, 2, 3, 4, 5 ]) Nothing
            , test "scanl 1" <|
                \_ ->
                    Expect.equalLists (Util.scanl (+) 0 [ 1, 2, 3 ]) [ 0, 1, 3, 6 ]
            , test "scanl 2" <|
                \_ ->
                    Expect.equalLists (Util.scanl (+) 0 (List.repeat 3 5)) [ 0, 5, 10, 15 ]
            , test "nthLast 1" <|
                \_ ->
                    Expect.equal (Util.nthLast 0 [ 0 ]) (Just 0)
            , test "nthLast 2" <|
                \_ ->
                    Expect.equal (Util.nthLast 4 [ 0, 1, 2, 3, 4 ]) (Just 0)
            , test "nthLast 4" <|
                \_ ->
                    Expect.equal (Util.nthLast 1 [ 0, 1, 2, 3, 4 ]) (Just 3)
            , test "nthLast 3" <|
                \_ ->
                    Expect.equal (Util.nthLast 44 [ 0, 1, 2, 3, 4 ]) Nothing
            , test "foo 1" <|
                \_ ->
                    Expect.equal
                        (Util.generateCombinations [ [ "x0", "x1" ], [ "y0", "y1", "y2" ], [ "z0", "z1" ] ])
                        [ [ "x0", "y0", "z0" ]
                        , [ "x0", "y0", "z1" ]
                        , [ "x0", "y1", "z0" ]
                        , [ "x0", "y1", "z1" ]
                        , [ "x0", "y2", "z0" ]
                        , [ "x0", "y2", "z1" ]
                        , [ "x1", "y0", "z0" ]
                        , [ "x1", "y0", "z1" ]
                        , [ "x1", "y1", "z0" ]
                        , [ "x1", "y1", "z1" ]
                        , [ "x1", "y2", "z0" ]
                        , [ "x1", "y2", "z1" ]
                        ]
            ]
        ]
