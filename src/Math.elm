module Math exposing (..)

import Point exposing (Point)


type Sign
    = Negative
    | Positive
    | None


sign : Float -> Sign
sign x =
    if x > 0 then
        Positive
    else if x < 0 then
        Negative
    else
        None


length : Point a -> Point b -> Float
length a b =
    sqrt <| (a.x - b.x) ^ 2 + (a.y - b.y) ^ 2


closestPoint : Point a -> Point b -> Point b -> Point b
closestPoint toPt pt1 pt2 =
    if length pt1 toPt <= length pt2 toPt then
        pt1
    else
        pt2


intersection : ( Point a, Point b ) -> ( Point c, Point d ) -> Point {}
intersection ( a1, a2 ) ( b1, b2 ) =
    let
        numeratorX =
            (a1.x * a2.y - a1.y * a2.x) * (b1.x - b2.x) - (a1.x - a2.x) * (b1.x * b2.y - b1.y * b2.x)

        denominatorX =
            (a1.x - a2.x) * (b1.y - b2.y) - (a1.y - a2.y) * (b1.x - b2.x)

        numeratorY =
            (a1.x * a2.y - a1.y * a2.x) * (b1.y - b2.y) - (a1.y - a2.y) * (b1.x * b2.y - b1.y * b2.x)

        denominatorY =
            (a1.x - a2.x) * (b1.y - b2.y) - (a1.y - a2.y) * (b1.x - b2.x)
    in
        { x = numeratorX / denominatorX
        , y = numeratorY / denominatorY
        }
