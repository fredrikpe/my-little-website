module Util exposing (all, any, contains, replaceIf)


any : (a -> Bool) -> List a -> Bool
any predicate list =
    List.foldl (\a b -> b || predicate a) False list


all : (a -> Bool) -> List a -> Bool
all predicate list =
    List.foldl (\a b -> b && predicate a) True list


contains : a -> List a -> Bool
contains x =
    any (\y -> y == x)


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
