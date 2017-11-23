module Point exposing (Point, point)


type alias Point a =
    { a
        | x : Float
        , y : Float
    }


point : Float -> Float -> Point {}
point x y =
    { x = x, y = y }
