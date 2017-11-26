module Point exposing (..)


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


fromPair : ( Float, Float ) -> Point {}
fromPair ( x, y ) =
    { x = x, y = y }
