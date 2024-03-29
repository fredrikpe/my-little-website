module Util exposing (all, any, contains, generateCombinations, getAt, indexOf, indexesOf, last, maybeLog, nthLast, remove, replaceIf, scanl, slice, splitEvery)

import List.Extra as Extra


any : (a -> Bool) -> List a -> Bool
any predicate list =
    List.foldl (\a b -> b || predicate a) False list


all : (a -> Bool) -> List a -> Bool
all predicate list =
    List.foldl (\a b -> b && predicate a) True list


contains : a -> List a -> Bool
contains x =
    any (\y -> y == x)


getAt : Int -> List a -> Maybe a
getAt i list =
    case list of
        x :: xs ->
            case i of
                0 ->
                    Just x

                n ->
                    getAt (n - 1) xs

        [] ->
            Nothing


replaceIf : (a -> Bool) -> a -> List a -> List a
replaceIf predicate value list =
    List.map
        (\x ->
            if predicate x then
                value

            else
                x
        )
        list


slice : Int -> Int -> List a -> List a
slice start end list =
    List.take (end - start) (List.drop start list)


indexOf : (a -> Bool) -> List a -> Maybe Int
indexOf f xs =
    let
        helper list n =
            case list of
                y :: ys ->
                    if f y then
                        Just n

                    else
                        helper ys (n + 1)

                [] ->
                    Nothing
    in
    helper xs 0


indexesOf : (a -> Bool) -> List a -> List Int
indexesOf f list =
    let
        helper xs n =
            case indexOf f xs of
                Just idx ->
                    [ n + idx ] ++ helper (List.drop (idx + 1) xs) (n + idx + 1)

                Nothing ->
                    []
    in
    helper list 0


scanl : (a -> b -> b) -> b -> List a -> List b
scanl fn b =
    let
        scan a bs =
            case bs of
                hd :: tl ->
                    fn a hd :: bs

                _ ->
                    []
    in
    List.foldl scan [ b ] >> List.reverse


last : List a -> Maybe a
last =
    List.foldl (Just >> always) Nothing


init : List a -> List a
init list =
    List.take (List.length list - 1) list


nthLast : Int -> List a -> Maybe a
nthLast n list =
    case n of
        0 ->
            last list

        m ->
            nthLast (m - 1) (init list)


remove : a -> List a -> List a
remove a list =
    List.filter (\x -> x /= a) list


generateCombinations : List (List a) -> List (List a)
generateCombinations input =
    let
        helper : List a -> List (List a) -> List (List a)
        helper acc list =
            case List.head list of
                Nothing ->
                    [ List.reverse acc ]

                Just head ->
                    let
                        tail =
                            case List.tail list of
                                Just data ->
                                    data

                                Nothing ->
                                    []
                    in
                    List.concat
                        (List.map
                            (\item ->
                                helper
                                    (item :: acc)
                                    tail
                            )
                            head
                        )
    in
    helper [] input


splitEvery : Int -> List a -> List (List a)
splitEvery n list =
    let
        ( head, tail ) =
            Extra.splitAt n list
    in
    case tail of
        [] ->
            [ head ]

        l ->
            [ head ] ++ splitEvery n l


maybeLog : String -> Maybe a -> Maybe a
maybeLog s m =
    case m of
        Just a ->
            let
                _ =
                    Debug.log s a
            in
            Just a

        Nothing ->
            Nothing
