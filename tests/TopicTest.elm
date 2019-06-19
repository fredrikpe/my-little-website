module TopicTest exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode as Decode
import Test exposing (..)
import Topic
import Util


suite : Test
suite =
    describe "The Topic module"
        [ describe "Decoders"
            [ test "partialDimensionDecoder" <|
                \_ ->
                    let
                        jsonString =
                            """
                            { "category": {
                                    "index": {
                                        "00": 0,
                                        "01": 1
                                    },
                                    "label": {
                                        "00": "t1",
                                        "01": "t2"
                                    }
                                },
                                "label": "type of adoption"
                            }
                            """

                        result =
                            Decode.decodeString Topic.partialDimensionDecoder jsonString

                        expected =
                            [ { value = "00", valueText = "t1", index = 0 }
                            , { value = "01", valueText = "t2", index = 1 }
                            ]
                    in
                    case result of
                        Ok list ->
                            Expect.equalLists list expected

                        Err _ ->
                            Expect.fail "Result was err"
            , test "datasetDataDecoder" <|
                \_ ->
                    let
                        jsonString =
                            """
                            {
                                "class": "dataset",
                                "dimension": {
                                    "Adopsjonstype": {
                                        "category": {
                                            "index": {
                                                "00": 0,
                                                "01": 1
                                            },
                                            "label": {
                                                "00": "Intercontry adoptions",
                                                "01": "Stepchildren"
                                            }
                                        },
                                        "label": "type of adoption"
                                    },
                                    "ContentsCode": {
                                        "category": {
                                            "index": {
                                                "Adopsjoner": 0
                                            },
                                            "label": {
                                                "Adopsjoner": "Adoptions"
                                            },
                                            "unit": {
                                                "Adopsjoner": {
                                                    "base": "adoptions",
                                                    "decimals": 0
                                                }
                                            }
                                        },
                                        "label": "contents"
                                    },
                                    "Tid": {
                                        "category": {
                                            "index": {
                                                "1986": 0,
                                                "1987": 1
                                            },
                                            "label": {
                                                "1986": "1986",
                                                "1987": "1987"
                                            }
                                        },
                                        "label": "year"
                                    }
                                },
                                "id": [
                                    "Adopsjonstype",
                                    "ContentsCode",
                                    "Tid"
                                ],
                                "label": "06683: Adoptions, by type of adoption, contents and year",
                                "role": {
                                    "metric": [
                                        "ContentsCode"
                                    ],
                                    "time": [
                                        "Tid"
                                    ]
                                },
                                "size": [
                                    2,
                                    1,
                                    2
                                ],
                                "source": "Statistics Norway",
                                "updated": "2019-05-31T06:00:00Z",
                                "value": [
                                    477,
                                    465,
                                    262,
                                    231
                                ],
                                "version": "2.0"
                            }
                            """

                        result =
                            Decode.decodeString Topic.datasetDataDecoder jsonString

                        expectedValues =
                            [ 477, 465, 262, 231 ]

                        expectedDimensions =
                            [ { code = "Adopsjonstype"
                              , text = "unknown"
                              , values =
                                    [ { value = "00", valueText = "Intercontry adoptions", index = 0 }
                                    , { value = "01", valueText = "Stepchildren", index = 1 }
                                    ]
                              }
                            , { code = "ContentsCode"
                              , text = "unknown"
                              , values =
                                    [ { value = "Adopsjoner", valueText = "Adoptions", index = 0 } ]
                              }
                            , { code = "Tid"
                              , text = "unknown"
                              , values =
                                    [ { value = "1986", valueText = "1986", index = 0 }
                                    , { value = "1987", valueText = "1987", index = 1 }
                                    ]
                              }
                            ]
                    in
                    case result of
                        Ok data ->
                            Expect.all
                                [ \s -> Expect.equalLists s.values expectedValues
                                , \s -> Expect.equalLists s.dimensions expectedDimensions
                                ]
                                data

                        Err _ ->
                            Expect.fail "Result was err"
            ]
        ]
