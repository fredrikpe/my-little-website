-- READ THIS
--https://discourse.elm-lang.org/t/using-task-to-send-http-requests/2696/5


module Main exposing (DatasetMsg(..), Model, Msg(..), configHtml, dimensionHtml, errorModel, handleDatasetMsg, hide, init, leafOnClick, main, notLoading, onQueryChange, querySelectOptions, setLoading, setQuery, show, treeHtml, treeListOnClick, update, updateTree, updateTrees, view, viewTree)

import Browser
import Dataset
import Html
import Html.Attributes
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
    , isLoading : Bool
    , query : Maybe Dataset.Query
    }


notLoading model =
    { model | isLoading = False }


updateTrees trees model =
    notLoading { model | trees = trees }


updateTree id f model =
    updateTrees (List.map (f id) model.trees) model


setLoading model =
    { model | isLoading = True }


errorModel errorMsg =
    { trees = [], errorMsg = Just errorMsg, isLoading = False, query = Nothing }


setQuery query model =
    { model | query = query }


show id =
    updateTree id (Dataset.setHidden False)


hide id =
    updateTree id (Dataset.setHidden True)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { trees = [], errorMsg = Nothing, isLoading = True, query = Nothing }
    , Task.attempt GotRoot (Dataset.getTree "")
    )


type DatasetMsg
    = DShow String Dataset.Config
    | DHide String
    | DGetConfig String
    | DGotConfig String (Result Http.Error Dataset.Config)
    | DSetQueryDimension Dataset.Dimension
    | DShowGraph
    | DGetData
    | DGotData (Result Http.Error Dataset.Dataset)


type Msg
    = Pass
    | ShowStrings (List String)
    | ShowQuery
    | GetRoot
    | GetSubTree String
    | DatasetMessage DatasetMsg
    | Show String
    | Hide String
    | GotRoot (Result Http.Error (List Dataset.Tree))
    | GotSubTree String (Result Http.Error (List Dataset.Tree))


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
                    ( { model | errorMsg = Just (Dataset.queryToString q) }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        GetRoot ->
            ( setLoading model, Task.attempt GotRoot (Dataset.getTree "") )

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
                    ( updateTrees trees model, Cmd.none )

                Err e ->
                    ( errorModel (Debug.toString e), Cmd.none )

        GotSubTree id result ->
            case result of
                Ok subTree ->
                    ( updateTree id
                        (\x y ->
                            Dataset.addSubTree subTree x (Dataset.setHidden False x y)
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
            ( (setQuery (Just (Dataset.blankQuery id config)) << show id) model, Cmd.none )

        DHide id ->
            ( (setQuery Nothing << hide id) model, Cmd.none )

        DGetData ->
            case model.query of
                Just q ->
                    ( setLoading model
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
            ( model
            , Task.attempt
                (\x ->
                    DatasetMessage (DGotConfig id x)
                )
                (Dataset.getLeafConfig id)
            )

        DGotConfig id result ->
            case result of
                Ok config ->
                    ( (setQuery (Just (Dataset.blankQuery id config))
                        << updateTree id
                            (\x y ->
                                Dataset.addLeafConfig config x (Dataset.setHidden False x y)
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
                    ( { model | errorMsg = Just (Dataset.queryToString q) }, Cmd.none )

                Nothing ->
                    ( errorModel "Query was Nothing when it shouldn't have been!", Cmd.none )


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.h2 [] [ Html.text "SSB Datasets" ]
        , if model.isLoading then
            Html.text "Loading..."

          else
            viewTree model
        ]


viewTree : Model -> Html.Html Msg
viewTree model =
    Html.div []
        [ Html.text (Maybe.withDefault "" model.errorMsg)
        , Html.button
            [ Html.Events.onClick GetRoot, Html.Attributes.style "display" "block" ]
            [ Html.text "Get Datasets!" ]
        , Html.ul [] (List.map treeHtml model.trees)
        ]


treeHtml : Dataset.Tree -> Html.Html Msg
treeHtml tree =
    case tree of
        Dataset.Category list ->
            Html.li []
                [ Html.button [ Html.Events.onClick (treeListOnClick list) ] [ Html.text list.text ]
                , Html.ul []
                    (if list.isHidden then
                        []

                     else
                        List.map treeHtml list.subTree
                    )
                ]

        Dataset.Leaf leaf ->
            Html.li []
                [ Html.button
                    [ Html.Events.onClick (leafOnClick leaf) ]
                    [ Html.text leaf.text ]
                , if leaf.isHidden then
                    Html.text ""

                  else
                    configHtml leaf.config
                ]


configHtml : Maybe Dataset.Config -> Html.Html Msg
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


treeListOnClick list =
    if List.length list.subTree == 0 then
        GetSubTree list.id

    else if list.isHidden then
        Show list.id

    else
        Hide list.id


leafOnClick leaf =
    case leaf.config of
        Just config ->
            if leaf.isHidden then
                DatasetMessage (DShow leaf.id config)

            else
                DatasetMessage (DHide leaf.id)

        Nothing ->
            DatasetMessage (DGetConfig leaf.id)
