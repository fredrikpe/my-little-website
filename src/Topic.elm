module Topic exposing (Config, TableAndConfig, TableData, Topic(..), Variable, addSubTopics, addTableConfig, configEncoder, getData, getTableConfig, getTopics, listConstructor, setHidden, ssbTopicsUrl, subListDecoder, tableConfigDecoder, tableConstructor, tableDataDecoder, tableDecoder, topicDecoder, topicListDecoder, variableDecoder)

import Http
import HttpUtil
import Json.Decode as Decode
import Json.Encode as Encode
import Task


ssbTopicsUrl =
    "http://data.ssb.no/api/v0/en/table/"


type Topic
    = TopicList { id : String, text : String, subTopics : List Topic, isHidden : Bool }
    | Table { id : String, text : String, config : Maybe Config, isHidden : Bool }


type alias TableAndConfig =
    { id : String, variables : List Variable }


type alias Config =
    { title : String, variables : List Variable }


type alias Variable =
    { code : String, text : String, values : List String, valueTexts : List String }


type alias TableData =
    { dummy : String }


listConstructor id text subTopics =
    TopicList { id = id, text = text, subTopics = subTopics, isHidden = False }


tableConstructor id text =
    Table { id = id, text = text, config = Nothing, isHidden = False }


getTopics : String -> Task.Task Http.Error (List Topic)
getTopics id =
    HttpUtil.httpGetFromJson (ssbTopicsUrl ++ id) (topicListDecoder id)


getTableConfig : String -> Task.Task Http.Error Config
getTableConfig id =
    HttpUtil.httpGetFromJson (ssbTopicsUrl ++ id) tableConfigDecoder


getData : String -> Config -> Task.Task Http.Error TableData
getData id config =
    HttpUtil.httpPostFromJson (ssbTopicsUrl ++ id) (configEncoder config) tableDataDecoder


addSubTopics : String -> List Topic -> Topic -> Topic
addSubTopics id subTopics topic =
    case topic of
        TopicList list ->
            if list.id == id then
                TopicList { list | subTopics = subTopics }

            else
                TopicList { list | subTopics = List.map (addSubTopics id subTopics) list.subTopics }

        _ ->
            topic


addTableConfig id config topic =
    case topic of
        TopicList list ->
            TopicList { list | subTopics = List.map (addTableConfig id config) list.subTopics }

        Table table ->
            if table.id == id then
                Table { table | config = Just config, isHidden = False }

            else
                topic


setHidden : String -> Bool -> Topic -> Topic
setHidden id bool topic =
    case topic of
        TopicList list ->
            if list.id == id then
                TopicList { list | isHidden = bool }

            else
                TopicList
                    { list | subTopics = List.map (setHidden id bool) list.subTopics }

        Table table ->
            if table.id == id then
                Table { table | isHidden = bool }

            else
                Table { table | isHidden = True }


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
                        tableDecoder

                    _ ->
                        Decode.fail "Couldn't decode error"
            )


tableDecoder : Decode.Decoder Topic
tableDecoder =
    Decode.map2 tableConstructor
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


tableConfigDecoder : Decode.Decoder Config
tableConfigDecoder =
    Decode.map2 Config
        (Decode.field "title" Decode.string)
        (Decode.field "variables" (Decode.list variableDecoder))


variableDecoder : Decode.Decoder Variable
variableDecoder =
    Decode.map4 Variable
        (Decode.field "code" Decode.string)
        (Decode.field "text" Decode.string)
        (Decode.field "values" (Decode.list Decode.string))
        (Decode.field "valueTexts" (Decode.list Decode.string))


tableDataDecoder : Decode.Decoder TableData
tableDataDecoder =
    Decode.map TableData
        (Decode.field "dummy" Decode.string)


configEncoder : Config -> Encode.Value
configEncoder config =
    Encode.object
        [ ( "query"
          , Encode.list
                (\v ->
                    Encode.object
                        [ ( "code", Encode.string v.code )
                        , ( "selection"
                          , Encode.object
                                [ ( "filter", Encode.string "item" )
                                , ( "values", Encode.list (\x -> Encode.string x) v.values )
                                ]
                          )
                        , ( "selection", Encode.object [] )
                        ]
                )
                config.variables
          )
        , ( "response", Encode.object [ ( "format", Encode.string "json-stat2" ) ] )
        ]
