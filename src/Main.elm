-- READ THIS
--https://discourse.elm-lang.org/t/using-task-to-send-http-requests/2696/5


module Main exposing (Model, Msg(..), init, main, showOrHide, topicHtml, update, view, viewTopics)

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
    = GetTopics
    | GetSubTopics Topic
    | ShowSubTopics Topic
    | HideSubTopics Topic
    | GotMainTopics (Result Http.Error (List Topic))
    | GotSubTopics String (Result Http.Error (List Topic))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetTopics ->
            ( { topics = [], title = "two" }, Task.attempt GotMainTopics getMainTopics )

        GetSubTopics (Topic topic) ->
            ( model, Task.attempt (GotSubTopics topic.id) (getSubTopics (Topic topic)) )

        ShowSubTopics (Topic topic) ->
            ( { topics = List.map (setHidden topic.id False) model.topics, title = "show" }, Cmd.none )

        HideSubTopics (Topic topic) ->
            ( { topics = List.map (setHidden topic.id True) model.topics, title = "hide" }, Cmd.none )

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
                    ( { topics = [], title = "x" }, Cmd.none )


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.h2 [] [ Html.text "Random Cats" ]
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
topicHtml (Topic topic) =
    Html.li []
        [ Html.button [ Html.Events.onClick (showOrHide (Topic topic)) ] [ Html.text topic.text ]
        , Html.ul []
            (if topic.isHidden then
                []

             else
                List.map topicHtml topic.subTopics
            )
        ]


showOrHide : Topic -> Msg
showOrHide (Topic topic) =
    if List.length topic.subTopics == 0 then
        GetSubTopics (Topic topic)

    else if topic.isHidden then
        ShowSubTopics (Topic topic)

    else
        HideSubTopics (Topic topic)
