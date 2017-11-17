module Level exposing (..)

import Point exposing (Point)
import List.Extra as List
import Collage
import Color
import Element


type alias Level =
    { walls : List Wall
    , cube : Maybe Point
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
    , cube = Just (Point 0 -240)
    }


levels : List Level
levels =
    [
        { walls =
            [ { orientation = Vertical
              , origin = Point 0 -250
              , length = 100
              }
            ]
        , cube = Nothing
        }
    ]


level : Int -> Level
level index =
    levels
        |> List.get (index - 1)
        |> Maybe.withDefault level0


view : Level -> List Collage.Form
view level =
    (level.cube
        |> Maybe.map cube
        |> Maybe.withDefault (Collage.toForm Element.empty))
    :: (List.map wall level.walls)


wall : Wall -> Collage.Form
wall w =
    Collage.segment
        ( w.origin.x, w.origin.y )
        (endPoint w)
            |> Collage.traced (Collage.solid Color.black)


cube : Point -> Collage.Form
cube c =
    Element.image 20 20 "./img/companion-cube.png"
        |> Collage.toForm
        |> Collage.move ( c.x, c.y )


endPoint : Wall -> ( Float, Float )
endPoint wall =
    case wall.orientation of
        Vertical ->
            ( wall.origin.x, wall.length + wall.origin.y )

        Horizontal ->
            ( wall.length + wall.origin.x, wall.origin.y )
