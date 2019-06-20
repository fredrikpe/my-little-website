module Util exposing (all, any, contains, generateCombinations, indexOf, last, replaceIf, scanl, slice)


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


indexOf : a -> List a -> Maybe Int
indexOf x xs =
    let
        f list n =
            case list of
                y :: ys ->
                    if y == x then
                        Just n

                    else
                        f ys (n + 1)

                [] ->
                    Nothing
    in
    f xs 0


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
