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
            ]
        ]
