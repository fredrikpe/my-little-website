module HttpUtil exposing (httpGetFromJson)

import Http
import Json.Decode as Decode
import Task


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
