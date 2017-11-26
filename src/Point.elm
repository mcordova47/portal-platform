module Point exposing (Point, point, toPair)


type alias Point a =
    { a
        | x : Float
        , y : Float
    }


point : Float -> Float -> Point {}
point x y =
    { x = x, y = y }


toPair : Point a -> ( Float, Float )
toPair { x, y } =
    ( x, y )
