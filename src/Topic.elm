module Topic exposing (Topic(..), addSubTopics, getMainTopics, getSubTopics, setHidden, ssbTopicsUrl, subTopicDecoder, subTopicListDecoder, topicDecoder, topicListDecoder)

import Http
import HttpUtil
import Json.Decode as Decode
import Task


ssbTopicsUrl =
    "http://data.ssb.no/api/v0/en/table/"


type Topic
    = Topic { id : String, text : String, subTopics : List Topic, isHidden : Bool }


topicConstructor : String -> String -> List Topic -> Topic
topicConstructor id text subTopics =
    Topic { id = id, text = text, subTopics = subTopics, isHidden = False }


getSubTopics : Topic -> Task.Task Http.Error (List Topic)
getSubTopics (Topic topic) =
    Http.task
        { url = ssbTopicsUrl ++ topic.id
        , method = "Get"
        , headers = []
        , body = Http.emptyBody
        , resolver = Http.stringResolver (HttpUtil.rr (subTopicListDecoder (Topic topic)))
        , timeout = Nothing
        }


getMainTopics : Task.Task Http.Error (List Topic)
getMainTopics =
    Http.task
        { url = ssbTopicsUrl
        , method = "Get"
        , headers = []
        , body = Http.emptyBody
        , resolver = Http.stringResolver (HttpUtil.rr topicListDecoder)
        , timeout = Nothing
        }


addSubTopics : String -> List Topic -> Topic -> Topic
addSubTopics id subTopics (Topic topic) =
    let
        y =
            Debug.log "asdf"
    in
    if topic.id == id then
        Topic { topic | subTopics = subTopics }

    else
        Topic topic


setHidden : String -> Bool -> Topic -> Topic
setHidden id bool (Topic topic) =
    if topic.id == id then
        Topic { topic | isHidden = bool }

    else
        Topic topic


subTopicListDecoder : Topic -> Decode.Decoder (List Topic)
subTopicListDecoder topic =
    Decode.list (subTopicDecoder topic)


topicListDecoder : Decode.Decoder (List Topic)
topicListDecoder =
    Decode.list topicDecoder


subTopicDecoder : Topic -> Decode.Decoder Topic
subTopicDecoder (Topic topic) =
    Decode.map3 topicConstructor
        (Decode.map (\s -> topic.id ++ "/" ++ s)
            (Decode.field
                "id"
                Decode.string
            )
        )
        (Decode.field "text" Decode.string)
        (Decode.succeed [])


topicDecoder : Decode.Decoder Topic
topicDecoder =
    Decode.map3 topicConstructor
        (Decode.field "id" Decode.string)
        (Decode.field "text" Decode.string)
        (Decode.succeed [])
