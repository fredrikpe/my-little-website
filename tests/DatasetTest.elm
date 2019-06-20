module DatasetTest exposing (suite)

import Dataset
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode as Decode
import Test exposing (..)
import Util


suite : Test
suite =
    describe "The Dataset module"
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
                            Decode.decodeString Dataset.partialDimensionDecoder jsonString

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
            , test "datasetDecoder" <|
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
                                                "00": "Intercontry",
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
                            Decode.decodeString Dataset.datasetDecoder jsonString

                        expectedValues =
                            [ 477, 465, 262, 231 ]

                        expectedDimensions =
                            [ { code = "Adopsjonstype"
                              , text = "unknown"
                              , values =
                                    [ { value = "00", valueText = "Intercontry", index = 0 }
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
            , test "Data access 2D" <|
                \_ ->
                    let
                        jsonString =
                            """
                            {
                                "dimension": {
                                    "x": {
                                        "category": {
                                            "index": { "x0": 0, "x1": 1 },
                                            "label": { "x0": "", "x1": "" }
                                        }
                                    },
                                    "y": {
                                        "category": {
                                            "index": { "y0": 0, "y1": 1, "y2": 2 },
                                            "label": { "y0": "", "y1": "", "y2": "" }
                                        }
                                    } 
                                },
                                "id": [ "x", "y" ],
                                "value": [ 0, 1, 2, 3, 4, 5 ]
                            }
                            """

                        result =
                            Decode.decodeString Dataset.datasetDecoder jsonString

                        values =
                            [ 0, 1, 2, 3, 4, 5 ]

                        x0 =
                            dimValue "x0" 0

                        x1 =
                            dimValue "x1" 1

                        y0 =
                            dimValue "y0" 0

                        y1 =
                            dimValue "y1" 1

                        y2 =
                            dimValue "y2" 2

                        x =
                            { code = "x", text = "unknown", values = [ x0, x1 ] }

                        y =
                            { code = "y", text = "unknown", values = [ y0, y1, y2 ] }

                        dimensions =
                            [ x, y ]
                    in
                    case result of
                        Ok data ->
                            Expect.all
                                [ \d -> Expect.equalLists d.values values
                                , \d -> Expect.equalLists d.dimensions dimensions
                                , \d -> Expect.pass

                                --, \d -> Expect.equalLists (Dataset.iterator d) []
                                --[ [ [ 0, 1, 2 ], [ 3, 4, 5 ], [] ], [] ]
                                --[ ( [ "x0" ], [ 0, 1, 2 ] ), ( [ "x1" ], [ 3, 4, 5 ] ) ]
                                ]
                                data

                        Err e ->
                            Expect.fail (Debug.toString e)
            , test "Data access 3D" <|
                \_ ->
                    let
                        jsonString =
                            """
                            {
                                "dimension": {
                                    "x": {
                                        "category": {
                                            "index": { "x0": 0, "x1": 1 },
                                            "label": { "x0": "", "x1": "" }
                                        }
                                    },
                                    "y": {
                                        "category": {
                                            "index": { "y0": 0, "y1": 1 },
                                            "label": { "y0": "", "y1": "" }
                                        }
                                    },
                                    "z": {
                                        "category": {
                                            "index": { "z0": 0, "z1": 1 },
                                            "label": { "z0": "", "z1": "" }
                                        }
                                    }
                                },
                                "id": [ "x", "y", "z" ],
                                "value": [ 0, 1, 2, 3, 4, 5, 6, 7 ]
                            }
                            """

                        result =
                            Decode.decodeString Dataset.datasetDecoder jsonString

                        values =
                            [ 0, 1, 2, 3, 4, 5, 6, 7 ]

                        x0 =
                            dimValue "x0" 0

                        x1 =
                            dimValue "x1" 1

                        y0 =
                            dimValue "y0" 0

                        y1 =
                            dimValue "y1" 1

                        z0 =
                            dimValue "z0" 0

                        z1 =
                            dimValue "z1" 1

                        x =
                            { code = "x", text = "unknown", values = [ x0, x1 ] }

                        y =
                            { code = "y", text = "unknown", values = [ y0, y1 ] }

                        z =
                            { code = "z", text = "unknown", values = [ z0, z1 ] }

                        dimensions =
                            [ x, y, z ]
                    in
                    case result of
                        Ok data ->
                            Expect.all
                                [ \d -> Expect.equalLists d.values values
                                , \d -> Expect.equalLists d.dimensions dimensions

                                --, \d -> Expect.equalLists (Dataset.iterator data) []
                                --[ ( [ "x0", "y0" ], [ 0, 1 ] )
                                --, ( [ "x0", "y1" ], [ 2, 3 ] )
                                --, ( [ "x1", "y0" ], [ 4, 5 ] )
                                --, ( [ "x1", "y1" ], [ 6, 7 ] )
                                --]
                                {-
                                   [ ("x0", ( "y0", [ 0, 1 ] )
                                   , ( [ "x0", "y1" ], [ 2, 3 ] )
                                   , ( [ "x1", "y0" ], [ 4, 5 ] )
                                   , ( [ "x1", "y1" ], [ 6, 7 ] )
                                   ]
                                -}
                                ]
                                data

                        Err e ->
                            Expect.fail (Debug.toString e)
            ]
        ]


dimValue s n =
    { value = s, valueText = "", index = n }
