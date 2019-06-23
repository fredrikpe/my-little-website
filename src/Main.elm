module Main exposing (main)

import Browser
import Chart
import Dataset
import Html exposing (Html, div, h1, node, p, text)
import Html.Attributes exposing (class)
import Html.Events
import Http
import HttpUtil
import Json.Decode as Decode
import MultiSelect
import Task
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
    { trees : List Dataset.Tree
    , errorMsg : Maybe String
    , query : Maybe Dataset.Query
    , hovered : Maybe Dataset.Point
    , datasetConfig : Maybe Dataset.Config
    , dataset : Maybe Dataset.Dataset
    , showTree : Bool
    }


defaultModel =
    { trees = []
    , errorMsg = Nothing
    , query = Nothing
    , hovered = Nothing
    , datasetConfig = Nothing
    , dataset = Nothing
    , showTree = False
    }


updateTree id f model =
    { model | trees = List.map (f id) model.trees }


errorModel errorMsg =
    { defaultModel | errorMsg = Just errorMsg }


setQuery query model =
    { model | query = query }


show id =
    updateTree id (Dataset.setHidden False)


hide id =
    updateTree id (Dataset.setHidden True)


init : () -> ( Model, Cmd Msg )
init _ =
    ( defaultModel, Task.attempt GotRoot (Dataset.getTree "") )


type DatasetMsg
    = DGetConfig String
    | DSetConfig (Maybe Dataset.Config)
    | DGotConfig String Dataset.Config
    | DSetQueryDimension Dataset.Dimension
    | DShowGraph
    | DGetData
    | DGotData (Result String Dataset.Dataset)


type Msg
    = Pass
    | ShowStrings (List String)
    | HttpError String
    | ShowQuery
    | GetRoot
    | ShowTree
    | GetSubTree String
    | DatasetMessage DatasetMsg
    | Show String
    | Hide String
    | GotRoot (Result String (List Dataset.Tree))
    | GotSubTree String (Result String (List Dataset.Tree))
    | Hover (Maybe Dataset.Point)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Pass ->
            ( model, Cmd.none )

        ShowTree ->
            let
                cmd =
                    if List.isEmpty model.trees then
                        Task.attempt GotRoot (Dataset.getTree "")

                    else
                        Cmd.none
            in
            ( { model | showTree = not model.showTree }, cmd )

        Hover hovered ->
            ( { model | hovered = hovered }, Cmd.none )

        ShowStrings s ->
            ( { model | errorMsg = Just (String.join "" s) }, Cmd.none )

        HttpError e ->
            ( { model | errorMsg = Just (Debug.toString e) }, Cmd.none )

        ShowQuery ->
            case model.query of
                Just q ->
                    ( { model | errorMsg = Just (Dataset.queryToString q) }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        GetRoot ->
            ( model, Task.attempt GotRoot (Dataset.getTree "") )

        GetSubTree id ->
            ( model, Task.attempt (GotSubTree id) (Dataset.getTree id) )

        Show id ->
            ( show id model, Cmd.none )

        Hide id ->
            ( hide id model, Cmd.none )

        DatasetMessage dMsg ->
            handleDatasetMsg dMsg model

        GotRoot result ->
            case result of
                Ok trees ->
                    ( { model | trees = trees, showTree = not model.showTree }, Cmd.none )

                Err e ->
                    ( errorModel (Debug.toString e), Cmd.none )

        GotSubTree id result ->
            case result of
                Ok subTree ->
                    ( updateTree id
                        (\x y ->
                            Dataset.setSubTree subTree x (Dataset.setHidden False x y)
                        )
                        model
                    , Cmd.none
                    )

                Err e ->
                    ( errorModel (Debug.toString e), Cmd.none )


handleDatasetMsg : DatasetMsg -> Model -> ( Model, Cmd Msg )
handleDatasetMsg msg model =
    case msg of
        DGetData ->
            case model.query of
                Just q ->
                    ( model
                    , Task.attempt
                        (\x ->
                            DatasetMessage (DGotData x)
                        )
                        (Dataset.getDataset q)
                    )

                Nothing ->
                    ( errorModel "Query was Nothing when trying to get data!", Cmd.none )

        DGotData result ->
            case result of
                Ok dataset ->
                    ( { model
                        | errorMsg = Just (Debug.toString dataset)
                        , dataset = Just dataset
                      }
                    , Cmd.none
                    )

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
                    ( errorModel "Query was Nothing when it shouldn't have been!"
                    , Cmd.none
                    )

        DGetConfig id ->
            ( model
            , Task.attempt
                (\result ->
                    case result of
                        Ok config ->
                            DatasetMessage (DGotConfig id config)

                        Err e ->
                            HttpError e
                )
                (Dataset.getLeafConfig id)
            )

        DGotConfig id config ->
            let
                newModel =
                    updateTree id
                        (Dataset.setLeafConfig config)
                        (setQuery (Just (Dataset.blankQuery id config)) model)
            in
            ( { newModel | datasetConfig = Just config, showTree = False }, Cmd.none )

        DSetConfig config ->
            ( { model | datasetConfig = config, showTree = config /= Nothing }, Cmd.none )

        DShowGraph ->
            case model.query of
                Just q ->
                    ( { model | errorMsg = Just (Dataset.queryToString q) }, Cmd.none )

                Nothing ->
                    ( errorModel "Query was Nothing when it shouldn't have been!"
                    , Cmd.none
                    )


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.h2 [] [ Html.text "SSB Datasets" ]
        , Html.button
            [ Html.Events.onClick ShowTree, Html.Attributes.style "display" "block" ]
            [ Html.text "Choose Dataset" ]
        , if model.showTree then
            viewTree model

          else
            viewChart model Hover
        ]


viewChart model msg =
    Html.div []
        [ configHtml model.datasetConfig
        , case model.dataset of
            Just d ->
                Chart.viewDataset d msg model.hovered

            Nothing ->
                Html.text ""
        ]


viewTree : Model -> Html.Html Msg
viewTree model =
    Html.div [] [ Html.ul [] (List.map (treeHtml model.datasetConfig) model.trees) ]


treeHtml : Maybe Dataset.Config -> Dataset.Tree -> Html.Html Msg
treeHtml oldConfig tree =
    case tree of
        Dataset.Category state list ->
            Html.li []
                [ Html.button [ Html.Events.onClick (treeListOnClick state list) ]
                    [ Html.text state.text ]
                , Html.ul []
                    (if state.isHidden then
                        []

                     else
                        List.map (treeHtml oldConfig) list
                    )
                ]

        Dataset.Leaf leaf ->
            Html.li []
                [ Html.button
                    [ Html.Events.onClick (leafOnClick leaf oldConfig) ]
                    [ Html.text leaf.text ]
                ]


configHtml : Maybe Dataset.Config -> Html.Html Msg
configHtml config =
    case config of
        Just c ->
            Html.div []
                [ Html.h3 [] [ Html.text c.title ]
                , Html.div [ Html.Attributes.class "view__config_div" ]
                    (List.map (\v -> dimensionHtml v) c.dimensions
                        ++ [ Html.button
                                [ Html.Events.onClick (DatasetMessage DGetData) ]
                                [ Html.text "Show graph" ]
                           ]
                    )
                ]

        Nothing ->
            Html.text ""


querySelectOptions : Dataset.Dimension -> MultiSelect.Options Msg
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


onQueryChange : Dataset.Dimension -> List String -> Msg
onQueryChange dimension s =
    DatasetMessage
        (DSetQueryDimension
            { dimension
                | values = List.filter (\v -> Util.any (\x -> x == v.value) s) dimension.values
            }
        )


treeListOnClick state list =
    if List.length list == 0 then
        GetSubTree state.id

    else if state.isHidden then
        Show state.id

    else
        Hide state.id


leafOnClick leaf oldConfig =
    case leaf.config of
        Just config ->
            if oldConfig == Just config then
                DatasetMessage (DSetConfig Nothing)

            else
                DatasetMessage (DSetConfig (Just config))

        Nothing ->
            DatasetMessage (DGetConfig leaf.id)


type alias Info =
    { age : Float
    , weight : Float
    , height : Float
    , income : Float
    }


alice : List Info
alice =
    [ Info 10 34 1.34 0
    , Info 16 42 1.62 3000
    , Info 25 75 1.73 25000
    , Info 43 83 1.75 40000
    ]


bobby : List Info
bobby =
    [ Info 10 38 1.32 0
    , Info 17 69 1.75 2000
    , Info 25 75 1.87 32000
    , Info 43 77 1.87 52000
    ]


chuck : List Info
chuck =
    [ Info 10 42 1.35 0
    , Info 15 72 1.72 1800
    , Info 25 89 1.83 85000
    , Info 43 95 1.84 120000
    ]
