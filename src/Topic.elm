module Topic exposing (Config, DatasetData, Query, DimValue, Dimension, Topic(..), addDatasetConfig, addSubTopics, blankQuery, datasetConfigDecoder, datasetConstructor, datasetDataDecoder, datasetDecoder, dimValueConstructor, dimensionDecoder, getData, getDatasetConfig, getTopics, helper001, helper002, listConstructor, partialDimensionDecoder, queryEncoder, queryToString, setHidden, ssbTopicsUrl, subListDecoder, topicDecoder, topicListDecoder)

import Http
import HttpUtil
import Json.Decode as Decode
import Json.Encode as Encode
import Task


ssbTopicsUrl =
    "http://data.ssb.no/api/v0/en/table/"


type DatasetTree
    = Category { id : String, text : String, subTopics : List DatasetTree, isHidden : Bool }
    | Leaf { id : String, text : String, config : Maybe Config, isHidden : Bool }


type alias Query =
    { id : String, dimensions : List Dimension }


type alias Config =
    { title : String, dimensions : List Dimension }


type alias Dimension =
    { code : String, text : String, values : List DimValue }


type alias DimValue =
    { value : String, valueText : String, index : Int }


type alias DatasetData =
    { dimensions : List Dimension, values : List Float }


listConstructor id text subTopics =
    Category { id = id, text = text, subTopics = subTopics, isHidden = False }


leafConstructor id text =
    Leaf { id = id, text = text, config = Nothing, isHidden = False }


dimValueConstructor value valueText =
    { value = value, valueText = valueText, index = -1 }


getTopics : String -> Task.Task Http.Error (List Topic)
getTopics id =
    HttpUtil.httpGetFromJson (ssbTopicsUrl ++ id) (topicListDecoder id)


getDatasetConfig : String -> Task.Task Http.Error Config
getDatasetConfig id =
    HttpUtil.httpGetFromJson (ssbTopicsUrl ++ id) datasetConfigDecoder


getData : Query -> Task.Task Http.Error DatasetData
getData query =
    HttpUtil.httpPostFromJson (ssbTopicsUrl ++ query.id) (queryEncoder query) datasetDataDecoder


addSubTopics : List Topic -> String -> Topic -> Topic
addSubTopics subTopics id topic =
    case topic of
        Category list ->
            if list.id == id then
                Category { list | subTopics = subTopics }

            else
                Category { list | subTopics = List.map (addSubTopics subTopics id) list.subTopics }

        _ ->
            topic


addDatasetConfig : Config -> String -> Topic -> Topic
addDatasetConfig config id topic =
    case topic of
        Category list ->
            Category { list | subTopics = List.map (addDatasetConfig config id) list.subTopics }

         dataset ->
            if dataset.id == id then
                Dataset { dataset | config = Just config, isHidden = False }

            else
                topic


setHidden : Bool -> String -> Topic -> Topic
setHidden bool id topic =
    case tree of
        Category list ->
            if list.id == id then
                Category { list | isHidden = bool }

            else
                Category
                    { list | subTopics = List.map (setHidden bool id) list.subTopics }

        Leaf dataset ->
            if Leaf.id == id then
                Leaf { dataset | isHidden = bool }

            else
                Leaf { dataset | isHidden = True }


topicListDecoder id =
    Decode.list (topicDecoder id)


topicDecoder id =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\t ->
                case t of
                    "l" ->
                        subListDecoder id

                    "t" ->
                        datasetDecoder

                    _ ->
                        Decode.fail "Couldn't decode error"
            )


datasetDecoder : Decode.Decoder Topic
datasetDecoder =
    Decode.map2 datasetConstructor
        (Decode.map
            (\s -> Maybe.withDefault "UNKNOWN" (List.head (String.split ":" s)))
            (Decode.field
                "text"
                Decode.string
            )
        )
        (Decode.field "text" Decode.string)


subListDecoder : String -> Decode.Decoder Topic
subListDecoder id =
    Decode.map3 listConstructor
        (Decode.map (\s -> id ++ "/" ++ s)
            (Decode.field
                "id"
                Decode.string
            )
        )
        (Decode.field "text" Decode.string)
        (Decode.succeed [])


datasetConfigDecoder : Decode.Decoder Config
datasetConfigDecoder =
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


datasetDataDecoder : Decode.Decoder DatasetData
datasetDataDecoder =
    Decode.map2 helper002
        (Decode.field "dimension" (Decode.keyValuePairs partialDimensionDecoder))
        (Decode.field "value" (Decode.list Decode.float))


helper002 : List ( String, List DimValue ) -> List Float -> DatasetData
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


