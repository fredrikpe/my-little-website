-- READ THIS
--https://discourse.elm-lang.org/t/using-task-to-send-http-requests/2696/5


module Main exposing (Model(..), Msg(..), getSubTopics, getTopics, init, main, renderList, subscriptions, topicDecoder, update, view, viewGif)

import Browser
import Html
import Html.Attributes
import Html.Events
import Http
import Json.Decode as Decode


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type Model
    = Failure
    | Loading
    | Success (List String)


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, Cmd.batch [ getSubTopic "al" ] )


type Msg
    = GetTopics
    | GotMainTopics (Result Http.Error (List String))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetTopics ->
            ( Loading, getTopics )

        GotMainTopics result ->
            case result of
                Ok topics ->
                    ( Success topics, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.h2 [] [ Html.text "Random Cats" ]
        , viewGif model
        ]


viewGif : Model -> Html.Html Msg
viewGif model =
    case model of
        Failure ->
            Html.div []
                [ Html.text "I could not load a random cat for some reason. "
                , Html.button [ Html.Events.onClick GetTopics ] [ Html.text "Try Again!" ]
                ]

        Loading ->
            Html.text "Loading..."

        Success response ->
            Html.div []
                [ Html.button
                    [ Html.Events.onClick GetTopics, Html.Attributes.style "display" "block" ]
                    [ Html.text "Get Topics!" ]
                , renderList response
                ]


renderList : List String -> Html.Html msg
renderList list =
    Html.ul []
        (List.map (\l -> Html.li [] [ Html.text l ]) list)



-- subTopics : List String -> List String
-- subTopics =
-- List.concatMap (\l -> subTopic l)
-- HTTP
--expectJson : (Result Http.Error a -> msg) -> Decoder a -> Expect msg


getSubTopics : List String -> List (Cmd Msg)
getSubTopics =
    List.map
        (\t -> getSubTopic t)


getSubTopic : String -> Cmd Msg
getSubTopic topic =
    Http.get
        { url = "http://data.ssb.no/api/v0/en/table/" ++ topic
        , expect = Http.expectJson GotMainTopics topicDecoder
        }


getTopics : Cmd Msg
getTopics =
    Http.get
        { url = "http://data.ssb.no/api/v0/en/table/"
        , expect = Http.expectJson GotMainTopics topicDecoder
        }


topicDecoder : Decode.Decoder (List String)
topicDecoder =
    Decode.list (Decode.field "id" Decode.string)
