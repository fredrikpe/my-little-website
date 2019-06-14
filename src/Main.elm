-- READ THIS
--https://discourse.elm-lang.org/t/using-task-to-send-http-requests/2696/5


module Main exposing (Model, Msg(..), init, main, tableOnClick, topicHtml, topicListOnClick, update, view, viewTopics)

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
    , isLoading : Bool
    , tableAndConfig : Maybe TableAndConfig
    }


notLoading model =
    { model | isLoading = False }


updateTopics model topics =
    notLoading { model | topics = topics }


setLoading model =
    { model | isLoading = True }


errorModel =
    { topics = [], title = "Error!", isLoading = False, tableAndConfig = Nothing }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { topics = [], title = "One", isLoading = False, tableAndConfig = Nothing }
    , Task.attempt GotMainTopics (Topic.getTopics "")
    )


type TableMsg
    = TblShow String
    | TblHide String
    | TblGetConfig String
    | TblGotConfig String (Result Http.Error Config)


type Msg
    = Pass
    | ShowStrings (List String)
    | GetTopics
    | GetSubTopics String
    | TableMessage TableMsg
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
            ( { model | title = String.join "" s }, Cmd.none )

        GetTopics ->
            ( model, Task.attempt GotMainTopics (getTopics "") )

        GetSubTopics id ->
            ( model, Task.attempt (GotSubTopics id) (getTopics id) )

        Show id ->
            ( updateTopics model (List.map (setHidden id False) model.topics), Cmd.none )

        Hide id ->
            ( updateTopics model (List.map (setHidden id True) model.topics), Cmd.none )

        TableMessage tblMsg ->
            handleTableMsg tblMsg model

        GotMainTopics result ->
            case result of
                Ok topics ->
                    ( updateTopics model topics, Cmd.none )

                Err _ ->
                    ( errorModel, Cmd.none )

        GotSubTopics id result ->
            case result of
                Ok subTopics ->
                    ( updateTopics model (List.map (addSubTopics id subTopics << setHidden id False) model.topics), Cmd.none )

                Err _ ->
                    ( errorModel, Cmd.none )


foo =
    12


handleTableMsg : TableMsg -> Model -> ( Model, Cmd Msg )
handleTableMsg msg model =
    case msg of
        TblShow id ->
            ( updateTopics model (List.map (setHidden id False) model.topics), Cmd.none )

        TblHide id ->
            ( updateTopics model (List.map (setHidden id True) model.topics), Cmd.none )

        TblGetConfig id ->
            ( model, Task.attempt (\x -> TableMessage (TblGotConfig id x)) (getTableConfig id) )

        TblGotConfig id result ->
            case result of
                Ok config ->
                    ( updateTopics model (List.map (addTableConfig id config << setHidden id False) model.topics), Cmd.none )

                Err _ ->
                    ( errorModel, Cmd.none )


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
                , if table.isHidden then
                    Html.text ""

                  else
                    configHtml table.config
                ]


configHtml : Maybe Config -> Html.Html Msg
configHtml config =
    case config of
        Just c ->
            Html.div [ Html.Attributes.class "view__config_div" ] (List.map (\v -> variableHtml v) c.variables)

        Nothing ->
            Html.text ""


valueSelectOptions : Variable -> MultiSelect.Options Msg
valueSelectOptions variable =
    let
        defaultOptions =
            MultiSelect.defaultOptions onChange
    in
    { defaultOptions
        | items =
            List.map (\v -> { value = v, text = v, enabled = True }) variable.valueTexts
    }


variableHtml variable =
    MultiSelect.multiSelect (valueSelectOptions variable)
        []
        []


onChange : List String -> Msg
onChange s =
    let
        _ =
            Debug.log "s" 12
    in
    ShowStrings s


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
                TableMessage (TblShow table.id)

            else
                TableMessage (TblHide table.id)

        Nothing ->
            TableMessage (TblGetConfig table.id)
