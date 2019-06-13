module HttpUtil exposing (responseToResult, rr)

import Http
import Json.Decode as Decode


responseToResult : Http.Response a -> Result Http.Error a
responseToResult response =
    case response of
        Http.GoodStatus_ metadata body ->
            Ok body

        Http.BadStatus_ medatada body ->
            Err (Http.BadStatus 1)

        Http.BadUrl_ string ->
            Err (Http.BadUrl string)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.NetworkError_ ->
            Err Http.NetworkError


rr : Decode.Decoder a -> Http.Response String -> Result Http.Error a
rr decoder response =
    case response of
        Http.GoodStatus_ metadata body ->
            let
                decoded =
                    Decode.decodeString decoder body
            in
            case decoded of
                Ok b ->
                    Ok b

                Err _ ->
                    Err (Http.BadStatus 1)

        _ ->
            Err (Http.BadStatus 1)
