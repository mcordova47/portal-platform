module Level exposing (..)

import Point exposing (Point)
import List.Extra as List
import Collage
import Color


type alias Level =
    { walls : List Wall
    }


type alias Wall =
    { orientation : Orientation
    , origin : Point
    , length : Float
    }


type Orientation
    = Vertical
    | Horizontal


level0 : Level
level0 =
    { walls = []
    }


levels : List Level
levels =
    [
        { walls =
            [ { orientation = Vertical
              , origin = Point 50 -250
              , length = 100
              }
            ]
        }
    ]


level : Int -> Level
level index =
    levels
        |> List.get (index - 1)
        |> Maybe.withDefault level0


view : Level -> List Collage.Form
view level =
    List.map wall level.walls


wall : Wall -> Collage.Form
wall w =
    Collage.segment
        ( w.origin.x, w.origin.y )
        (endPoint w)
            |> Collage.traced (Collage.solid Color.black)


endPoint : Wall -> ( Float, Float )
endPoint wall =
    case wall.orientation of
        Vertical ->
            ( wall.origin.x, wall.length + wall.origin.y )

        Horizontal ->
            ( wall.length + wall.origin.x, wall.origin.y )
