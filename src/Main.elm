-- READ THIS
--https://discourse.elm-lang.org/t/using-task-to-send-http-requests/2696/5


module Main exposing (Model, Msg(..), init, main, tableOnClick, topicHtml, topicListOnClick, update, view, viewTopics)

import Browser
import Html
import Html.Attributes
import Html.Events
import Http
import HttpUtil
import Task
import Topic exposing (..)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }


type alias Model =
    { topics : List Topic
    , title : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { topics = [], title = "One" }, Task.attempt GotMainTopics Topic.getMainTopics )


type Msg
    = Pass
    | GetTopics
    | GetSubTopics Topic
    | ShowSubTopics Topic
    | HideSubTopics Topic
    | GotMainTopics (Result Http.Error (List Topic))
    | GotSubTopics Topic (Result Http.Error (List Topic))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Pass ->
            ( model, Cmd.none )

        GetTopics ->
            ( { topics = [], title = "two" }, Task.attempt GotMainTopics getMainTopics )

        GetSubTopics topic ->
            ( model, Task.attempt (GotSubTopics topic) (getSubTopics topic) )

        ShowSubTopics topic ->
            ( { topics = List.map (setHidden topic False) model.topics, title = "show" }, Cmd.none )

        HideSubTopics topic ->
            ( { topics = List.map (setHidden topic True) model.topics, title = "hide" }, Cmd.none )

        GotMainTopics result ->
            case result of
                Ok topics ->
                    ( { topics = topics, title = "three" }, Cmd.none )

                Err _ ->
                    ( { topics = [], title = "89" }, Cmd.none )

        GotSubTopics topic result ->
            case result of
                Ok subTopics ->
                    ( { topics = List.map (addSubTopics topic subTopics) model.topics, title = "four" }, Cmd.none )

                Err _ ->
                    ( { topics = [], title = "x" }, Cmd.none )


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.h2 [] [ Html.text "SSB Datasets" ]
        , viewTopics model
        ]


viewTopics : Model -> Html.Html Msg
viewTopics model =
    case List.length model.topics of
        0 ->
            Html.text "Loading..."

        _ ->
            Html.div []
                [ Html.text model.title
                , Html.button
                    [ Html.Events.onClick GetTopics, Html.Attributes.style "display" "block" ]
                    [ Html.text "Get Topics!" ]
                , Html.ul [] (List.map topicHtml model.topics)
                ]


topicHtml : Topic -> Html.Html Msg
topicHtml topic =
    case topic of
        TopicList list ->
            Html.li []
                [ Html.button [ Html.Events.onClick (topicListOnClick list) ] [ Html.text list.text ]
                , Html.ul []
                    (if list.isHidden then
                        []

                     else
                        List.map topicHtml list.subTopics
                    )
                ]

        Table table ->
            Html.li [] [ Html.button [ Html.Events.onClick (tableOnClick table) ] [ Html.text table.text ] ]



-- How to write this signature?


topicListOnClick list =
    if List.length list.subTopics == 0 then
        GetSubTopics (TopicList list)

    else if list.isHidden then
        ShowSubTopics (TopicList list)

    else
        HideSubTopics (TopicList list)


tableOnClick list =
    Pass
