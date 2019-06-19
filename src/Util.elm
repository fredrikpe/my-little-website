module Util exposing (all, any, contains, indexOf, replaceIf, slice)


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
                    x

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


foldl2 : (a -> a -> b -> b) -> b -> List a -> List a -> b
foldl2 f initialValue =
    let
        helper acc l1 l2 =
            case ( l1, l2 ) of
                ( x1 :: xs1, x2 :: xs2 ) ->
                    helper (f x1 x2 acc) xs1 xs2

                _ ->
                    acc
    in
    helper initialValue


andThen : (a -> List b) -> List a -> List b
andThen =
    List.concatMap


lift2 : (a -> b -> c) -> List a -> List b -> List c
lift2 f la lb =
    la |> andThen (\a -> lb |> andThen (\b -> [ f a b ]))


cartesianProduct : List (List a) -> List (List a)
cartesianProduct ll =
    case ll of
        [] ->
            [ [] ]

        xs :: xss ->
            lift2 (::) xs (cartesianProduct xss)


type alias DataArray =
    List Float


type alias Tensor =
    { data : DataArray
    , shape : List Int
    }


stridesFromShape : Int -> List Int -> List Int
stridesFromShape dimension shape =
    scanl (*) 1 shape
        |> List.take dimension


extractTensorValues : List Int -> List Int -> DataArray -> DataArray
extractTensorValues shape strides data =
    let
        subToIndex : List Int -> Int
        subToIndex =
            foldl2 (\stride s acc -> stride * s + acc) 0 strides

        allIndices : List Int
        allIndices =
            shape
                |> List.foldl (\s ranges -> List.range 0 (s - 1) :: ranges) []
                |> cartesianProduct
                |> List.map (List.reverse >> subToIndex)
    in
    extractIndices allIndices data


extractIndices : List Int -> DataArray -> DataArray
extractIndices indices data =
    List.map (\i -> getAt i data) indices
        |> List.filter
            (\x ->
                case x of
                    Just n ->
                        True

                    Nothing ->
                        False
            )
