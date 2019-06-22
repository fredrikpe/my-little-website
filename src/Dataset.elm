module Dataset exposing (Chart, Config, Dataset, DimValue, Dimension, Line, Point, PointConverter, Query, Tree(..), blankQuery, categoryConstructor, datasetDecoder, dateConverter, dimValueConstructor, dimensionDecoder, firstLongDimensionSize, getDataset, getLeafConfig, getTree, helper001, helper002, leafConfigDecoder, leafConstructor, leafDecoder, makeCharts, partialDimensionDecoder, queryEncoder, queryToString, setHidden, setLeafConfig, setSubTree, splitToCharts, splitToLines, ssbTreesUrl, subListDecoder, treeDecoder, treeListDecoder)

import Http
import HttpUtil
import Json.Decode as Decode
import Json.Encode as Encode
import Task
import Util


ssbTreesUrl =
    "http://data.ssb.no/api/v0/en/table/"


type Tree
    = Category { id : String, text : String, isHidden : Bool } (List Tree)
    | Leaf { id : String, text : String, config : Maybe Config }


type alias Query =
    { id : String, dimensions : List Dimension }


type alias Config =
    { title : String, dimensions : List Dimension }


type alias Dimension =
    { code : String, text : String, values : List DimValue }


type alias DimValue =
    { value : String, valueText : String, index : Int }


type alias Dataset =
    { dimensions : List Dimension, values : List Float }


type alias Chart =
    { lines : List Line }


type alias Line =
    { points : List Point }


type alias Point =
    { x : String, y : Float }


type alias PointConverter =
    { xToFloat : String -> Float, xToString : Float -> String }


firstLongDimensionSize dataset =
    Util.indexOf (\dim -> List.length dim.values > 1) dataset.dimensions
        |> Maybe.andThen (\idx -> Util.getAt idx dataset.dimensions)
        |> Maybe.map (\dim -> List.length dim.values)
        |> Maybe.withDefault 1


splitToCharts : Int -> List Line -> List (List Line)
splitToCharts n lines =
    Util.splitEvery n lines


splitToLines : Dataset -> Result String (List Line)
splitToLines dataset =
    Result.fromMaybe "No last dim!" (Util.last dataset.dimensions)
        |> Result.map
            (\lastDim ->
                List.map
                    (\values ->
                        { points =
                            List.map2 Point
                                (List.map (\x -> x.value) lastDim.values)
                                values
                        }
                    )
                    (Util.splitEvery (List.length lastDim.values) dataset.values)
            )


makeCharts : Dataset -> Result String (List (List Line))
makeCharts dataset =
    let
        dimsNotOne =
            Util.indexesOf (\dim -> List.length dim.values /= 1) dataset.dimensions

        firstSize =
            firstLongDimensionSize dataset
    in
    case List.length dimsNotOne of
        2 ->
            Result.map (\lines -> splitToCharts firstSize lines) (splitToLines dataset)

        1 ->
            Result.map (\lines -> splitToCharts 1 lines) (splitToLines dataset)

        _ ->
            Err "Wrong size of dataset"


dateConverter : Dataset -> (String -> Float)
dateConverter dataset =
    case Util.last dataset.dimensions of
        Just dim ->
            \s ->
                Util.indexOf (\x -> x == s) (List.map (\v -> v.value) dim.values)
                    |> Maybe.map toFloat
                    |> Maybe.withDefault 1

        Nothing ->
            \s -> 1


categoryConstructor id text subTree =
    Category { id = id, text = text, isHidden = False } subTree


leafConstructor id text =
    Leaf { id = id, text = text, config = Nothing }


dimValueConstructor value valueText =
    { value = value, valueText = valueText, index = -1 }


getTree : String -> Task.Task Http.Error (List Tree)
getTree id =
    HttpUtil.httpGetFromJson (ssbTreesUrl ++ id) (treeListDecoder id)


getLeafConfig : String -> Task.Task Http.Error Config
getLeafConfig id =
    HttpUtil.httpGetFromJson (ssbTreesUrl ++ id) leafConfigDecoder


getDataset : Query -> Task.Task Http.Error Dataset
getDataset query =
    HttpUtil.httpPostFromJson
        (ssbTreesUrl ++ query.id)
        (queryEncoder query)
        datasetDecoder


setSubTree : List Tree -> String -> Tree -> Tree
setSubTree subTree id tree =
    case tree of
        Category state list ->
            if state.id == id then
                Category state subTree

            else
                Category state (List.map (setSubTree subTree id) list)

        _ ->
            tree


setLeafConfig : Config -> String -> Tree -> Tree
setLeafConfig config id tree =
    case tree of
        Category state list ->
            Category state (List.map (setLeafConfig config id) list)

        Leaf leaf ->
            if leaf.id == id then
                Leaf { leaf | config = Just config }

            else
                tree


setHidden : Bool -> String -> Tree -> Tree
setHidden bool id tree =
    case tree of
        Category state list ->
            if state.id == id then
                Category { state | isHidden = bool } list

            else
                Category state (List.map (setHidden bool id) list)

        l ->
            l


treeListDecoder id =
    Decode.list (treeDecoder id)


treeDecoder id =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\t ->
                case t of
                    "l" ->
                        subListDecoder id

                    "t" ->
                        leafDecoder

                    _ ->
                        Decode.fail "Couldn't decode error"
            )


leafDecoder : Decode.Decoder Tree
leafDecoder =
    Decode.map2 leafConstructor
        (Decode.map
            (\s -> Maybe.withDefault "UNKNOWN" (List.head (String.split ":" s)))
            (Decode.field
                "text"
                Decode.string
            )
        )
        (Decode.field "text" Decode.string)


subListDecoder : String -> Decode.Decoder Tree
subListDecoder id =
    Decode.map3 categoryConstructor
        (Decode.map (\s -> id ++ "/" ++ s)
            (Decode.field
                "id"
                Decode.string
            )
        )
        (Decode.field "text" Decode.string)
        (Decode.succeed [])


leafConfigDecoder : Decode.Decoder Config
leafConfigDecoder =
    Decode.map2 Config
        (Decode.field "title" Decode.string)
        (Decode.field "variables" (Decode.list dimensionDecoder))


dimensionDecoder : Decode.Decoder Dimension
dimensionDecoder =
    Decode.map3 Dimension
        (Decode.field "code" Decode.string)
        (Decode.field "text" Decode.string)
        (Decode.map2
            (List.map2 dimValueConstructor)
            (Decode.field "values" (Decode.list Decode.string))
            (Decode.field "valueTexts" (Decode.list Decode.string))
        )


partialDimensionDecoder : Decode.Decoder (List DimValue)
partialDimensionDecoder =
    Decode.field "category"
        (Decode.map2
            helper001
            (Decode.field "index" (Decode.keyValuePairs Decode.int))
            (Decode.field "label" (Decode.keyValuePairs Decode.string))
        )


helper001 : List ( String, Int ) -> List ( String, String ) -> List DimValue
helper001 index label =
    let
        values =
            List.map (\l -> { value = Tuple.first l, valueText = Tuple.second l, index = -1 }) label

        unsorted =
            List.map
                (\d ->
                    let
                        found =
                            List.head (List.filter (\t -> Tuple.first t == d.value) index)
                    in
                    case found of
                        Just f ->
                            { d | index = Tuple.second f }

                        Nothing ->
                            d
                )
                values
    in
    List.sortBy .index unsorted


datasetDecoder : Decode.Decoder Dataset
datasetDecoder =
    Decode.map2 helper002
        (Decode.field "dimension" (Decode.keyValuePairs partialDimensionDecoder))
        (Decode.field "value" (Decode.list Decode.float))


helper002 : List ( String, List DimValue ) -> List Float -> Dataset
helper002 keyValueList values =
    { values = values
    , dimensions = List.map (\t -> { code = Tuple.first t, text = "unknown", values = Tuple.second t }) keyValueList
    }


queryEncoder : Query -> Encode.Value
queryEncoder query =
    Encode.object
        [ ( "query"
          , Encode.list
                (\v ->
                    Encode.object
                        [ ( "code", Encode.string v.code )
                        , ( "selection"
                          , Encode.object
                                [ ( "filter", Encode.string "item" )
                                , ( "values"
                                  , Encode.list
                                        (\x ->
                                            Encode.string x.value
                                        )
                                        v.values
                                  )
                                ]
                          )
                        ]
                )
                query.dimensions
          )
        , ( "response", Encode.object [ ( "format", Encode.string "json-stat2" ) ] )
        ]


queryToString query =
    Encode.encode 4 (queryEncoder query)


blankQuery : String -> Config -> Query
blankQuery id config =
    { id = id, dimensions = List.map (\v -> { v | values = [] }) config.dimensions }
