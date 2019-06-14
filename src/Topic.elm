module Topic exposing (TableConfig, Topic(..), Variable, addSubTopics, addTableConfig, getTableConfig, getTopics, httpGetFromJson, listConstructor, setHidden, ssbTopicsUrl, subListDecoder, tableConfigDecoder, tableConstructor, tableDecoder, topicDecoder, topicListDecoder, variableDecoder)

import Http
import HttpUtil
import Json.Decode as Decode
import Task


ssbTopicsUrl =
    "http://data.ssb.no/api/v0/en/table/"


type Topic
    = TopicList { id : String, text : String, subTopics : List Topic, isHidden : Bool }
    | Table { id : String, text : String, config : Maybe TableConfig, isHidden : Bool }


type alias TableConfig =
    { title : String, variables : List Variable }


type alias Variable =
    { code : String, text : String, values : List String, valueTexts : List String }


listConstructor id text subTopics =
    TopicList { id = id, text = text, subTopics = subTopics, isHidden = False }


tableConstructor id text =
    Table { id = id, text = text, config = Nothing, isHidden = False }


httpGetFromJson : String -> Decode.Decoder a -> Task.Task Http.Error a
httpGetFromJson url decoder =
    Http.task
        { url = url
        , method = "Get"
        , headers = []
        , body = Http.emptyBody
        , resolver = Http.stringResolver (HttpUtil.responseToResult decoder)
        , timeout = Nothing
        }


getTopics : String -> Task.Task Http.Error (List Topic)
getTopics id =
    httpGetFromJson (ssbTopicsUrl ++ id) (topicListDecoder id)


getTableConfig : String -> Task.Task Http.Error TableConfig
getTableConfig id =
    httpGetFromJson (ssbTopicsUrl ++ id) tableConfigDecoder


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
                Table { table | config = Just config }

            else
                topic


setHidden : String -> Bool -> Topic -> Topic
setHidden id bool topic =
    case topic of
        TopicList list ->
            if list.id == id then
                TopicList { list | isHidden = bool }

            else
                TopicList { list | subTopics = List.map (setHidden id bool) list.subTopics }

        Table table ->
            if table.id == id then
                Table { table | isHidden = bool }

            else
                topic


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


tableConfigDecoder : Decode.Decoder TableConfig
tableConfigDecoder =
    Decode.map2 TableConfig
        (Decode.field "title" Decode.string)
        (Decode.field "variables" (Decode.list variableDecoder))


variableDecoder : Decode.Decoder Variable
variableDecoder =
    Decode.map4 Variable
        (Decode.field "code" Decode.string)
        (Decode.field "text" Decode.string)
        (Decode.field "values" (Decode.list Decode.string))
        (Decode.field "valueTexts" (Decode.list Decode.string))
