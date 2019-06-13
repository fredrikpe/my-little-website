module Topic exposing (Topic(..), addSubTopics, getMainTopics, getSubTopics, listConstructor, mainListDecoder, mainTopicListDecoder, setHidden, ssbTopicsUrl, subListDecoder, subTopicListDecoder, tableConstructor, tableDecoder, topicToString, updateTopic)

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


getSubTopics : Topic -> Task.Task Http.Error (List Topic)
getSubTopics topic =
    case topic of
        TopicList list ->
            Http.task
                { url = ssbTopicsUrl ++ list.id
                , method = "Get"
                , headers = []
                , body = Http.emptyBody
                , resolver = Http.stringResolver (HttpUtil.responseToResult (subTopicListDecoder (TopicList list)))
                , timeout = Nothing
                }

        _ ->
            Task.fail (Http.BadStatus 3)


getMainTopics : Task.Task Http.Error (List Topic)
getMainTopics =
    Http.task
        { url = ssbTopicsUrl
        , method = "Get"
        , headers = []
        , body = Http.emptyBody
        , resolver = Http.stringResolver (HttpUtil.responseToResult mainTopicListDecoder)
        , timeout = Nothing
        }


updateTopic : Topic -> (Topic -> Topic) -> Topic -> Topic
updateTopic old f topic =
    case old of
        TopicList oldList ->
            case topic of
                TopicList list ->
                    if list.id == oldList.id then
                        f old

                    else
                        TopicList { list | subTopics = List.map (updateTopic old f) list.subTopics }

                _ ->
                    topic

        _ ->
            topic


addSubTopics : Topic -> List Topic -> Topic -> Topic
addSubTopics old subTopics topic =
    let
        f old_ =
            case old_ of
                TopicList list ->
                    TopicList { list | subTopics = subTopics }

                _ ->
                    old_
    in
    updateTopic old f topic


setHidden : Topic -> Bool -> Topic -> Topic
setHidden old bool topic =
    let
        f old_ =
            case old_ of
                TopicList list ->
                    TopicList { list | isHidden = bool }

                _ ->
                    old_
    in
    updateTopic old f topic


subTopicListDecoder : Topic -> Decode.Decoder (List Topic)
subTopicListDecoder topic =
    Decode.list (Decode.oneOf [ tableDecoder topic, subListDecoder topic ])


mainTopicListDecoder : Decode.Decoder (List Topic)
mainTopicListDecoder =
    Decode.list mainListDecoder


tableDecoder : Topic -> Decode.Decoder Topic
tableDecoder topic =
    case topic of
        _ ->
            Decode.map2 tableConstructor
                (Decode.map (\s -> Maybe.withDefault "UNKNOWN" (List.head (String.split ":" s)))
                    (Decode.field
                        "text"
                        Decode.string
                    )
                )
                (Decode.field "text" Decode.string)


subListDecoder : Topic -> Decode.Decoder Topic
subListDecoder topic =
    case topic of
        TopicList list ->
            Decode.map3 listConstructor
                (Decode.map (\s -> list.id ++ "/" ++ s)
                    (Decode.field
                        "id"
                        Decode.string
                    )
                )
                (Decode.field "text" Decode.string)
                (Decode.succeed [])

        _ ->
            Decode.fail "Unreachable. Trying to decode sub lists for a table!"


mainListDecoder : Decode.Decoder Topic
mainListDecoder =
    Decode.map3 listConstructor
        (Decode.field "id" Decode.string)
        (Decode.field "text" Decode.string)
        (Decode.succeed [])
