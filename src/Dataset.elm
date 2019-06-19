module Dataset exposing (Config, Dataset, DimValue, Dimension, Query, Tree(..), addLeafConfig, addSubTree, blankQuery, categoryConstructor, datasetDecoder, dimValueConstructor, dimensionDecoder, getDataset, getLeafConfig, getTree, helper001, helper002, leafConfigDecoder, leafConstructor, leafDecoder, partialDimensionDecoder, queryEncoder, queryToString, setHidden, ssbTreesUrl, subListDecoder, treeDecoder, treeListDecoder)

import Http
import HttpUtil
import Json.Decode as Decode
import Json.Encode as Encode
import Task


ssbTreesUrl =
    "http://data.ssb.no/api/v0/en/table/"


type Tree
    = Category { id : String, text : String, subTree : List Tree, isHidden : Bool }
    | Leaf { id : String, text : String, config : Maybe Config, isHidden : Bool }


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


categoryConstructor id text subTree =
    Category { id = id, text = text, subTree = subTree, isHidden = False }


leafConstructor id text =
    Leaf { id = id, text = text, config = Nothing, isHidden = False }


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


addSubTree : List Tree -> String -> Tree -> Tree
addSubTree subTree id tree =
    case tree of
        Category list ->
            if list.id == id then
                Category { list | subTree = subTree }

            else
                Category { list | subTree = List.map (addSubTree subTree id) list.subTree }

        _ ->
            tree


addLeafConfig : Config -> String -> Tree -> Tree
addLeafConfig config id tree =
    case tree of
        Category list ->
            Category { list | subTree = List.map (addLeafConfig config id) list.subTree }

        Leaf leaf ->
            if leaf.id == id then
                Leaf { leaf | config = Just config, isHidden = False }

            else
                tree


setHidden : Bool -> String -> Tree -> Tree
setHidden bool id tree =
    case tree of
        Category list ->
            if list.id == id then
                Category { list | isHidden = bool }

            else
                Category
                    { list | subTree = List.map (setHidden bool id) list.subTree }

        Leaf leaf ->
            if leaf.id == id then
                Leaf { leaf | isHidden = bool }

            else
                Leaf { leaf | isHidden = True }


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
    in
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