module Topic exposing (Topic(..), addSubTopics, getTopics, listConstructor, setHidden, ssbTopicsUrl, subListDecoder, tableConstructor, tableDecoder, topicDecoder, topicListDecoder, topicToString)

import Http
import HttpUtil
import Json.Decode as Decode
import Task


ssbTopicsUrl =
    "http://data.ssb.no/api/v0/en/table/"


type Topic
    = TopicList { id : String, text : String, subTopics : List Topic, isHidden : Bool }
    | Table { id : String, text : String }


listConstructor id text subTopics =
    TopicList { id = id, text = text, subTopics = subTopics, isHidden = False }


tableConstructor id text =
    Table { id = id, text = text }


topicToString topic =
    case topic of
        TopicList t ->
            t.text

        Table t ->
            t.text


getTopics : String -> Task.Task Http.Error (List Topic)
getTopics id =
    Http.task
        { url = ssbTopicsUrl ++ id
        , method = "Get"
        , headers = []
        , body = Http.emptyBody
        , resolver = Http.stringResolver (HttpUtil.responseToResult (topicListDecoder id))
        , timeout = Nothing
        }


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


setHidden : String -> Bool -> Topic -> Topic
setHidden id bool topic =
    case topic of
        TopicList list ->
            if list.id == id then
                TopicList { list | isHidden = bool }

            else
                TopicList { list | subTopics = List.map (setHidden id bool) list.subTopics }

        _ ->
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
