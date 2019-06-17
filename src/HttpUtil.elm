module HttpUtil exposing (httpGetFromJson, httpPostFromJson)

import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Task


corsAnywhere =
    "https://cors-anywhere.herokuapp.com/"


httpPostFromJson : String -> Encode.Value -> Decode.Decoder a -> Task.Task Http.Error a
httpPostFromJson url body decoder =
    Http.task
        { url = corsAnywhere ++ url
        , method = "POST"
        , headers = []
        , body = Http.jsonBody body
        , resolver = Http.stringResolver (responseToResult decoder)
        , timeout = Nothing
        }


httpGetFromJson : String -> Decode.Decoder a -> Task.Task Http.Error a
httpGetFromJson url decoder =
    Http.task
        { url = url
        , method = "Get"
        , headers = []
        , body = Http.emptyBody
        , resolver = Http.stringResolver (responseToResult decoder)
        , timeout = Nothing
        }


responseToResult : Decode.Decoder a -> Http.Response String -> Result Http.Error a
responseToResult decoder response =
    case response of
        Http.GoodStatus_ metadata body ->
            case Decode.decodeString decoder body of
                Ok decoded ->
                    Ok decoded

                Err _ ->
                    Err (Http.BadStatus 2)

        Http.BadStatus_ medatada body ->
            Err (Http.BadStatus 1)

        Http.BadUrl_ string ->
            Err (Http.BadUrl string)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.NetworkError_ ->
            Err Http.NetworkError
