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
    ( { topics = [], title = "One" }, Task.attempt GotMainTopics (Topic.getTopics "") )


type Msg
    = Pass
    | GetTopics
    | GetSubTopics String
    | GetTableConfig String
    | Show String
    | Hide String
    | GotMainTopics (Result Http.Error (List Topic))
    | GotSubTopics String (Result Http.Error (List Topic))
    | GotTableConfig String (Result Http.Error TableConfig)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Pass ->
            ( model, Cmd.none )

        GetTopics ->
            ( { topics = [], title = "two" }, Task.attempt GotMainTopics (getTopics "") )

        GetSubTopics id ->
            ( model, Task.attempt (GotSubTopics id) (getTopics id) )

        GetTableConfig id ->
            ( model, Task.attempt (GotTableConfig id) (getTableConfig id) )

        Show id ->
            ( { topics = List.map (setHidden id False) model.topics, title = "show" }, Cmd.none )

        Hide id ->
            ( { topics = List.map (setHidden id True) model.topics, title = "hide" }, Cmd.none )

        GotMainTopics result ->
            case result of
                Ok topics ->
                    ( { topics = topics, title = "three" }, Cmd.none )

                Err _ ->
                    ( { topics = [], title = "89" }, Cmd.none )

        GotSubTopics id result ->
            case result of
                Ok subTopics ->
                    ( { topics = List.map (addSubTopics id subTopics) model.topics, title = "four" }, Cmd.none )

                Err _ ->
                    ( { topics = [], title = "error" }, Cmd.none )

        GotTableConfig id result ->
            case result of
                Ok config ->
                    ( { topics = List.map (addTableConfig id config) model.topics, title = "four" }, Cmd.none )

                Err _ ->
                    ( { topics = [], title = "error" }, Cmd.none )


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
            Html.li []
                [ Html.button [ Html.Events.onClick (tableOnClick table) ] [ Html.text table.text ]
                , Html.div []
                    (if table.isHidden then
                        []

                     else
                        [ configHtml table.config ]
                    )
                ]


configHtml : Maybe TableConfig -> Html.Html Msg
configHtml config =
    case config of
        Just c ->
            Html.text c.title

        Nothing ->
            Html.text ""



-- How to write this signature?


topicListOnClick list =
    if List.length list.subTopics == 0 then
        GetSubTopics list.id

    else if list.isHidden then
        Show list.id

    else
        Hide list.id


tableOnClick table =
    case table.config of
        Just config ->
            if table.isHidden then
                Show table.id

            else
                Hide table.id

        Nothing ->
            GetTableConfig table.id
