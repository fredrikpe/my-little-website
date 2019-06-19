-- READ THIS
--https://discourse.elm-lang.org/t/using-task-to-send-http-requests/2696/5


module Main exposing (DatasetMsg(..), Model, Msg(..), configHtml, datasetOnClick, errorModel, handleDatasetMsg, hide, init, main, notLoading, onQueryChange, querySelectOptions, setLoading, setQuery, show, topicHtml, topicListOnClick, update, updateTopic, updateTopics, view, viewTopics)

import Browser
import Html
import Html.Attributes
import Html.Events
import Http
import HttpUtil
import Json.Decode as Decode
import MultiSelect
import Task
import Topic exposing (..)
import Util


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
    , errorMsg : Maybe String
    , isLoading : Bool
    , query : Maybe DatasetQuery
    }


notLoading model =
    { model | isLoading = False }


updateTopics topics model =
    notLoading { model | topics = topics }


updateTopic id f model =
    updateTopics (List.map (f id) model.topics) model


setLoading model =
    { model | isLoading = True }


errorModel errorMsg =
    { topics = [], errorMsg = Just errorMsg, isLoading = False, query = Nothing }


setQuery query model =
    { model | query = query }


show id =
    updateTopic id (setHidden False)


hide id =
    updateTopic id (setHidden True)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { topics = [], errorMsg = Nothing, isLoading = True, query = Nothing }
    , Task.attempt GotMainTopics (Topic.getTopics "")
    )


type DatasetMsg
    = DShow String Config
    | DHide String
    | DGetConfig String
    | DGotConfig String (Result Http.Error Config)
    | DSetQueryDimension Dimension
    | DShowGraph
    | DGetData
    | DGotData (Result Http.Error DatasetData)


type Msg
    = Pass
    | ShowStrings (List String)
    | ShowQuery
    | GetTopics
    | GetSubTopics String
    | DatasetMessage DatasetMsg
    | Show String
    | Hide String
    | GotMainTopics (Result Http.Error (List Topic))
    | GotSubTopics String (Result Http.Error (List Topic))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Pass ->
            ( model, Cmd.none )

        ShowStrings s ->
            ( { model | errorMsg = Just (String.join "" s) }, Cmd.none )

        ShowQuery ->
            case model.query of
                Just q ->
                    ( { model | errorMsg = Just (queryToString q) }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        GetTopics ->
            ( setLoading model, Task.attempt GotMainTopics (getTopics "") )

        GetSubTopics id ->
            ( model, Task.attempt (GotSubTopics id) (getTopics id) )

        Show id ->
            ( show id model, Cmd.none )

        Hide id ->
            ( hide id model, Cmd.none )

        DatasetMessage dMsg ->
            handleDatasetMsg dMsg model

        GotMainTopics result ->
            case result of
                Ok topics ->
                    ( updateTopics topics model, Cmd.none )

                Err e ->
                    ( errorModel (Debug.toString e), Cmd.none )

        GotSubTopics id result ->
            case result of
                Ok subTopics ->
                    ( updateTopic id
                        (\x y ->
                            addSubTopics subTopics x (setHidden False x y)
                        )
                        model
                    , Cmd.none
                    )

                Err e ->
                    ( errorModel (Debug.toString e), Cmd.none )


handleDatasetMsg : DatasetMsg -> Model -> ( Model, Cmd Msg )
handleDatasetMsg msg model =
    case msg of
        DShow id config ->
            ( (setQuery (Just (blankQuery id config)) << show id) model, Cmd.none )

        DHide id ->
            ( (setQuery Nothing << hide id) model, Cmd.none )

        DGetData ->
            case model.query of
                Just q ->
                    ( setLoading model, Task.attempt (\x -> DatasetMessage (DGotData x)) (getData q) )

                Nothing ->
                    ( errorModel "Query was Nothing when trying to get data!", Cmd.none )

        DGotData result ->
            case result of
                Ok data ->
                    ( notLoading { model | errorMsg = Just (Debug.toString data) }, Cmd.none )

                Err e ->
                    ( errorModel (Debug.toString e), Cmd.none )

        DSetQueryDimension dimension ->
            case model.query of
                Just q ->
                    let
                        dims =
                            Util.replaceIf
                                (\d ->
                                    d.code == dimension.code
                                )
                                dimension
                                q.dimensions
                    in
                    ( setQuery (Just { q | dimensions = dims }) model, Cmd.none )

                Nothing ->
                    ( errorModel "Query was Nothing when it shouldn't have been!", Cmd.none )

        DGetConfig id ->
            ( model, Task.attempt (\x -> DatasetMessage (DGotConfig id x)) (getDatasetConfig id) )

        DGotConfig id result ->
            case result of
                Ok config ->
                    ( (setQuery (Just (blankQuery id config))
                        << updateTopic id
                            (\x y ->
                                addDatasetConfig config x (setHidden False x y)
                            )
                      )
                        model
                    , Cmd.none
                    )

                Err e ->
                    ( errorModel (Debug.toString e), Cmd.none )

        DShowGraph ->
            case model.query of
                Just q ->
                    ( { model | errorMsg = Just (queryToString q) }, Cmd.none )

                Nothing ->
                    ( errorModel "Query was Nothing when it shouldn't have been!", Cmd.none )


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.h2 [] [ Html.text "SSB Datasets" ]
        , if model.isLoading then
            Html.text "Loading..."

          else
            viewTopics model
        ]


viewTopics : Model -> Html.Html Msg
viewTopics model =
    Html.div []
        [ Html.text (Maybe.withDefault "" model.errorMsg)
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

        Dataset dataset ->
            Html.li []
                [ Html.button
                    [ Html.Events.onClick (datasetOnClick dataset) ]
                    [ Html.text dataset.text ]
                , if dataset.isHidden then
                    Html.text ""

                  else
                    configHtml dataset.config
                ]


configHtml : Maybe Config -> Html.Html Msg
configHtml config =
    case config of
        Just c ->
            Html.div [ Html.Attributes.class "view__config_div" ]
                (List.map (\v -> dimensionHtml v) c.dimensions
                    ++ [ Html.button
                            [ Html.Events.onClick (DatasetMessage DGetData) ]
                            [ Html.text "Show graph" ]
                       ]
                )

        Nothing ->
            Html.text ""


querySelectOptions : Dimension -> MultiSelect.Options Msg
querySelectOptions dimension =
    let
        defaultOptions =
            MultiSelect.defaultOptions (onQueryChange dimension)
    in
    { defaultOptions
        | items =
            List.map
                (\v ->
                    { value = v.value, text = v.valueText, enabled = True }
                )
                dimension.values
    }


dimensionHtml dimension =
    MultiSelect.multiSelect (querySelectOptions dimension)
        []
        []


onQueryChange : Dimension -> List String -> Msg
onQueryChange dimension s =
    DatasetMessage
        (DSetQueryDimension
            { dimension
                | values = List.filter (\v -> Util.any (\x -> x == v.value) s) dimension.values
            }
        )


topicListOnClick list =
    if List.length list.subTopics == 0 then
        GetSubTopics list.id

    else if list.isHidden then
        Show list.id

    else
        Hide list.id


datasetOnClick dataset =
    case dataset.config of
        Just config ->
            if dataset.isHidden then
                DatasetMessage (DShow dataset.id config)

            else
                DatasetMessage (DHide dataset.id)

        Nothing ->
            DatasetMessage (DGetConfig dataset.id)
