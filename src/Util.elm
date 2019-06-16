module Util exposing (replaceIf)


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
