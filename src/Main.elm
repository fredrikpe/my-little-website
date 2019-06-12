module Main exposing (Model(..), Msg(..), getRandomCatGif, init, main, renderList, subTopics, subscriptions, topicDecoder, update, view, viewGif)

import Browser
import Html
import Html.Attributes
import Html.Events
import Http
import Json.Decode as Decode



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type Model
    = Failure
    | Loading
    | Success (List String)


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, getRandomCatGif )



-- UPDATE


type Msg
    = MorePlease
    | GotGif (Result Http.Error (List String))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MorePlease ->
            ( Loading, getRandomCatGif )

        GotGif result ->
            case result of
                Ok url ->
                    ( Success url, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


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
                , Html.button [ Html.Events.onClick MorePlease ] [ Html.text "Try Again!" ]
                ]

        Loading ->
            Html.text "Loading..."

        Success response ->
            Html.div []
                [ Html.button
                    [ Html.Events.onClick MorePlease, Html.Attributes.style "display" "block" ]
                    [ Html.text "More Please!" ]
                , renderList response
                ]


renderList : List String -> Html.Html msg
renderList list =
    Html.ul []
        (List.map (\l -> Html.li [] [ Html.text l ]) list)


subTopic : String -> List String
subTopic topic =
    Http.get
        { url = "http://data.ssb.no/api/v0/en/table/" ++ topic
        , expect = Http.expectJson GotGif topicDecoder
        }


subTopics : List String -> List String
subTopics =
    List.concatMap (\l -> subTopic l)



-- HTTP


getRandomCatGif : Cmd Msg
getRandomCatGif =
    Http.get
        { url = "http://data.ssb.no/api/v0/en/table/"
        , expect = Http.expectJson GotGif topicDecoder
        }


topicDecoder : Decode.Decoder (List String)
topicDecoder =
    Decode.list (Decode.field "id" Decode.string)
