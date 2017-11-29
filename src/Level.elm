module Level exposing (Level, Wall, Orientation(..), level, endPoint, view)

import Point exposing (Point)
import List.Extra as List
import Collage
import Color
import Element
import Text


type alias Level =
    { walls : List Wall
    , cube : Maybe (Point {})
    , cake : Point {}
    , index : Int
    , end : Bool
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
    , cake = { x = 230, y = -238 }
    , index = 0
    , end = False
    }


levels : List Level
levels =
    [ { walls =
            [ { orientation = Vertical
              , origin = { x = 0, y = -250 }
              , length = 100
              }
            ]
      , cube = Nothing
      , cake = { x = 230, y = -238 }
      , index = 1
      , end = False
      }
    , { walls =
            [ { orientation = Horizontal
              , origin = { x = 0, y = 0 }
              , length = 250
              }
            ]
      , cube = Nothing
      , cake = { x = 230, y = 12 }
      , index = 2
      , end = False
      }
    , { walls =
            [ -- RIGHT SIDE
              { orientation = Horizontal
              , origin = { x = -150, y = -150 }
              , length = 400
              }
            , { orientation = Horizontal
              , origin = { x = -150, y = -100 }
              , length = 400
              }
            , { orientation = Horizontal
              , origin = { x = -150, y = -50 }
              , length = 400
              }
            , { orientation = Horizontal
              , origin = { x = -150, y = 0 }
              , length = 400
              }
            , { orientation = Horizontal
              , origin = { x = -150, y = 50 }
              , length = 400
              }
            , { orientation = Horizontal
              , origin = { x = -150, y = 100 }
              , length = 400
              }
            , { orientation = Horizontal
              , origin = { x = -150, y = 150 }
              , length = 400
              }
            , { orientation = Horizontal
              , origin = { x = -150, y = 200 }
              , length = 400
              }

            -- LEFT SIDE
            , { orientation = Horizontal
              , origin = { x = -250, y = -125 }
              , length = 400
              }
            , { orientation = Horizontal
              , origin = { x = -250, y = -75 }
              , length = 400
              }
            , { orientation = Horizontal
              , origin = { x = -250, y = -25 }
              , length = 400
              }
            , { orientation = Horizontal
              , origin = { x = -250, y = 25 }
              , length = 400
              }
            , { orientation = Horizontal
              , origin = { x = -250, y = 75 }
              , length = 400
              }
            , { orientation = Horizontal
              , origin = { x = -250, y = 125 }
              , length = 400
              }
            , { orientation = Horizontal
              , origin = { x = -250, y = 175 }
              , length = 400
              }
            ]
      , cube = Nothing
      , cake = { x = 230, y = 212 }
      , index = 3
      , end = False
      }
    , { walls =
            [ { orientation = Vertical
              , origin = { x = 0, y = -250 }
              , length = 100
              }
            , { orientation = Vertical
              , origin = { x = 0, y = -149 }
              , length = 399
              }
            ]
      , cube = Nothing
      , cake = { x = 230, y = -238 }
      , index = 4
      , end = False
      }
    , { walls = []
      , cube = Nothing
      , cake = { x = 230, y = -238 }
      , index = 5
      , end = True
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
    [ cube level.cube
    , cake level.cake
    , endText level.end
    ]
        ++ (List.map wall level.walls)


wall : Wall -> Collage.Form
wall w =
    Collage.segment
        ( w.origin.x, w.origin.y )
        (endPoint w)
        |> Collage.traced (Collage.solid Color.black)


cube : Maybe (Point a) -> Collage.Form
cube position =
    position
        |> Maybe.map (image 20 20 "./img/companion-cube.png")
        |> Maybe.withDefault (Collage.toForm Element.empty)


cake : Point a -> Collage.Form
cake =
    image 30 30 "./img/cake.jpg"


endText : Bool -> Collage.Form
endText end =
    if end then
        Text.fromString "The End"
            |> Text.monospace
            |> Text.height 24
            |> Element.centered
            |> Collage.toForm
    else
        (Collage.toForm Element.empty)


image : Int -> Int -> String -> Point a -> Collage.Form
image width height url point =
    Element.image width height url
        |> Collage.toForm
        |> Collage.move ( point.x, point.y )


endPoint : Wall -> ( Float, Float )
endPoint wall =
    case wall.orientation of
        Vertical ->
            ( wall.origin.x, wall.length + wall.origin.y )

        Horizontal ->
            ( wall.length + wall.origin.x, wall.origin.y )
