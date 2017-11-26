module Maybe.Extra exposing (orElse, foldMap)


orElse : Maybe a -> Maybe a -> Maybe a
orElse maybe1 maybe2 =
    case maybe2 of
        Nothing ->
            maybe1

        _ ->
            maybe2


foldMap : (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
foldMap fn maybeA maybeB =
    Maybe.map2 fn maybeA maybeB
        |> orElse maybeB
        |> orElse maybeA
