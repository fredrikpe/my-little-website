module Topic exposing (Config, DatasetData, DatasetQuery, Topic(..), Variable, addDatasetConfig, addSubTopics, blankQuery, datasetConfigDecoder, datasetConstructor, datasetDataDecoder, datasetDecoder, getData, getDatasetConfig, getTopics, listConstructor, queryEncoder, queryToString, setHidden, ssbTopicsUrl, subListDecoder, topicDecoder, topicListDecoder, variableDecoder)

import Http
import HttpUtil
import Json.Decode as Decode
import Json.Encode as Encode
import Task


ssbTopicsUrl =
    "http://data.ssb.no/api/v0/en/table/"


type Topic
    = TopicList { id : String, text : String, subTopics : List Topic, isHidden : Bool }
    | Dataset { id : String, text : String, config : Maybe Config, isHidden : Bool }


type alias DatasetQuery =
    { id : String, variables : List Variable }


type alias Config =
    { title : String, variables : List Variable }


type alias Variable =
    { code : String, text : String, values : List ( String, String ) }


type alias DatasetData =
    { dummy : String }


listConstructor id text subTopics =
    TopicList { id = id, text = text, subTopics = subTopics, isHidden = False }


datasetConstructor id text =
    Dataset { id = id, text = text, config = Nothing, isHidden = False }


getTopics : String -> Task.Task Http.Error (List Topic)
getTopics id =
    HttpUtil.httpGetFromJson (ssbTopicsUrl ++ id) (topicListDecoder id)


getDatasetConfig : String -> Task.Task Http.Error Config
getDatasetConfig id =
    HttpUtil.httpGetFromJson (ssbTopicsUrl ++ id) datasetConfigDecoder


getData : DatasetQuery -> Task.Task Http.Error DatasetData
getData query =
    HttpUtil.httpPostFromJson (ssbTopicsUrl ++ query.id) (queryEncoder query) datasetDataDecoder


addSubTopics : List Topic -> String -> Topic -> Topic
addSubTopics subTopics id topic =
    case topic of
        TopicList list ->
            if list.id == id then
                TopicList { list | subTopics = subTopics }

            else
                TopicList { list | subTopics = List.map (addSubTopics subTopics id) list.subTopics }

        _ ->
            topic


addDatasetConfig : Config -> String -> Topic -> Topic
addDatasetConfig config id topic =
    case topic of
        TopicList list ->
            TopicList { list | subTopics = List.map (addDatasetConfig config id) list.subTopics }

        Dataset dataset ->
            if dataset.id == id then
                Dataset { dataset | config = Just config, isHidden = False }

            else
                topic



--setAllHidden : Bool -> String -> Topic -> Topic


setHidden : Bool -> String -> Topic -> Topic
setHidden bool id topic =
    case topic of
        TopicList list ->
            if list.id == id then
                TopicList { list | isHidden = bool }

            else
                TopicList
                    { list | subTopics = List.map (setHidden bool id) list.subTopics }

        Dataset dataset ->
            if dataset.id == id then
                Dataset { dataset | isHidden = bool }

            else
                Dataset { dataset | isHidden = True }


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
        (Decode.field "variables" (Decode.list variableDecoder))


variableDecoder : Decode.Decoder Variable
variableDecoder =
    Decode.map3 Variable
        (Decode.field "code" Decode.string)
        (Decode.field "text" Decode.string)
        (Decode.map2
            (List.map2 Tuple.pair)
            (Decode.field "values" (Decode.list Decode.string))
            (Decode.field "valueTexts" (Decode.list Decode.string))
        )


datasetDataDecoder : Decode.Decoder DatasetData
datasetDataDecoder =
    Decode.map DatasetData Decode.string


queryEncoder : DatasetQuery -> Encode.Value
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
                                            Encode.string (Tuple.first x)
                                        )
                                        v.values
                                  )
                                ]
                          )
                        ]
                )
                query.variables
          )
        , ( "response", Encode.object [ ( "format", Encode.string "json-stat2" ) ] )
        ]


queryToString query =
    Encode.encode 4 (queryEncoder query)


blankQuery : String -> Config -> DatasetQuery
blankQuery id config =
    { id = id, variables = List.map (\v -> { v | values = [] }) config.variables }
