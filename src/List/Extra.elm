module List.Extra exposing (..)


get : Int -> List a -> Maybe a
get index list =
    list
        |> List.indexedMap (\i v -> ( i, v ))
        |> List.filter (\(i, v) -> i == index)
        |> List.head
        |> Maybe.map (\(i, v) -> v)
