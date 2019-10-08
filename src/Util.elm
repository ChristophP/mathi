module Util exposing (listCount)


listCount : (a -> Bool) -> List a -> Int
listCount fn xs =
    List.foldl
        (\current count ->
            if fn current then
                count + 1

            else
                count
        )
        0
        xs
