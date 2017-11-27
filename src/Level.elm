module Level exposing (Level, Wall, Orientation(..), level, endPoint, view)

import Point exposing (Point)
import List.Extra as List
import Collage
import Color
import Element


type alias Level =
    { walls : List Wall
    , cube : Maybe (Point {})
    }


type alias Wall =
    { orientation : Orientation
    , origin : Point {}
    , length : Float
    }


type Orientation
    = Vertical
    | Horizontal


baseWalls : List Wall
baseWalls =
    [ { orientation = Horizontal
      , origin = { x = -250, y = 250 }
      , length = 500
      }
    , { orientation = Vertical
      , origin = { x = 250, y = -250 }
      , length = 500
      }
    , { orientation = Horizontal
      , origin = { x = -250, y = -250 }
      , length = 500
      }
    , { orientation = Vertical
      , origin = { x = -250, y = -250 }
      , length = 500
      }
    ]


level0 : Level
level0 =
    { walls = []
    , cube = Nothing
    }


levels : List Level
levels =
    [ { walls =
            [ { orientation = Vertical
              , origin = { x = 0, y = -250 }
              , length = 100
              }
            , { orientation = Horizontal
              , origin = { x = 100, y = 0 }
              , length = 150
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
        |> addBorder


addBorder : Level -> Level
addBorder level =
    { level | walls = level.walls ++ baseWalls }


view : Level -> List Collage.Form
view level =
    (level.cube
        |> Maybe.map cube
        |> Maybe.withDefault (Collage.toForm Element.empty)
    )
        :: (List.map wall level.walls)


wall : Wall -> Collage.Form
wall w =
    Collage.segment
        ( w.origin.x, w.origin.y )
        (endPoint w)
        |> Collage.traced (Collage.solid Color.black)


cube : Point a -> Collage.Form
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
