module Main exposing (..)

import Html exposing (Html)
import Collage
import Element
import Color exposing (Color)
import Window
import Task
import Mouse
import Keyboard
import Time exposing (Time)
import AnimationFrame


-- MODEL


type alias Model =
    { size : Window.Size
    , player : Player
    , activeGun : Portal
    , target : Point
    , bluePortal : Maybe Point
    , orangePortal : Maybe Point
    }


type alias Player =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    }


type alias Point =
    { x : Float
    , y : Float
    }


type Portal
    = Blue
    | Orange


type KeyPress
    = Up Int
    | Down Int


init : ( Model, Cmd Msg )
init =
    ( { size = Window.Size 0 0
      , player =
          { x = 0
          , y = -240
          , vx = 0
          , vy = 0
          }
      , activeGun = Blue
      , target =
          { x = 0
          , y = 0
          }
      , bluePortal = Nothing
      , orangePortal = Nothing
      }
    , Task.perform SetSize Window.size
    )



-- UPDATE


type Msg
    = Tick Time
    | SetSize Window.Size
    | MoveMouse Mouse.Position
    | KeyUp Int
    | KeyDown Int
    | Click


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick diff ->
            ( step diff model, Cmd.none )

        SetSize size ->
            ( { model | size = size }, Cmd.none )

        MoveMouse position ->
            ( setTarget position model, Cmd.none )

        KeyDown code ->
            ( onKeyPress (Down code) model, Cmd.none )

        KeyUp code ->
            ( onKeyPress (Up code) model, Cmd.none )

        Click ->
            ( shootPortal model, Cmd.none )


shootPortal : Model -> Model
shootPortal model =
    case model.activeGun of
        Blue ->
            { model | bluePortal = Just model.target }

        Orange ->
            { model | orangePortal = Just model.target }


setTarget : Mouse.Position -> Model -> Model
setTarget position model =
    let
        mouseX =
            (toFloat (position.x * 2 - model.size.width)) / 2

        mouseY =
            (toFloat (model.size.height - position.y * 2)) / 2

        mousePos =
            { x = mouseX, y = mouseY }

        playerPos =
            { x = model.player.x, y = model.player.y }

        intersect =
            if mouseX < model.player.x then
                intersection
                    ( playerPos, mousePos )
                    ( { x = -250, y = -250 }, { x = -250, y = 250 } )
            else
                intersection
                    ( playerPos, mousePos )
                    ( { x = 250, y = -250 }, { x = 250, y = 250 } )

        target =
            if intersect.y > 250 then
                intersection
                    ( playerPos, mousePos )
                    ( { x = -250, y = 250 }, { x = 250, y = 250 } )
            else if intersect.y < -250 then
                intersection
                    ( playerPos, mousePos )
                    ( { x = -250, y = -250 }, { x = 250, y = -250 } )
            else
                intersect
    in
        { model | target = target }


intersection : ( Point, Point ) -> ( Point, Point ) -> Point
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


step : Time -> Model -> Model
step diff model =
    move diff model


move : Time -> Model -> Model
move diff ({ player } as model) =
    let
        seconds =
            Time.inSeconds diff

        movedPlayer =
            { player
                | x = player.x + player.vx * seconds
                , y = player.y + player.vy * seconds
            }
    in
        { model | player = movedPlayer }


velocity : Float
velocity =
    100


onKeyPress : KeyPress -> Model -> Model
onKeyPress keyPress ({ player, activeGun } as model) =
    case keyPress of
        Down 37 ->
            { model | player = { player | vx = -velocity } }

        Up 37 ->
            { model | player = { player | vx = 0 } }

        Down 65 ->
            { model | player = { player | vx = -velocity } }

        Up 65 ->
            { model | player = { player | vx = 0 } }

        Down 39 ->
            { model | player = { player | vx = velocity } }

        Up 39 ->
            { model | player = { player | vx = 0 } }

        Down 68 ->
            { model | player = { player | vx = velocity } }

        Up 68 ->
            { model | player = { player | vx = 0 } }

        Down 38 ->
            { model
                | activeGun =
                    if activeGun == Blue then
                        Orange
                    else
                        Blue
            }

        _ ->
            model


-- VIEW


view : Model -> Html Msg
view model =
    board model
        |> Collage.collage
            model.size.width
            model.size.height
        |> Element.toHtml



board : Model -> List Collage.Form
board { player, target, bluePortal, orangePortal, activeGun } =
    [ Collage.rect 500 500
        |> Collage.outlined (Collage.solid Color.black)
    , Collage.circle 10
        |> Collage.filled Color.gray
        |> Collage.move ( player.x, player.y )
    , Collage.segment
        ( player.x, player.y )
        ( target.x, target.y )
            |> Collage.traced (Collage.dashed (portalColor activeGun))
    ]
    ++ (portals bluePortal orangePortal)


portalColor : Portal -> Color
portalColor portal =
    case portal of
        Blue ->
            Color.blue

        Orange ->
            Color.orange


portals : Maybe Point -> Maybe Point -> List Collage.Form
portals bluePortal orangePortal =
    case ( bluePortal, orangePortal ) of
        ( Nothing, Nothing ) ->
            []

        ( Nothing, Just { x, y } ) ->
            [ Collage.oval 10 50
                |> Collage.filled (portalColor Orange)
                |> Collage.move ( x, y )
            ]

        ( Just { x, y }, Nothing ) ->
            [ Collage.oval 10 50
                |> Collage.filled (portalColor Blue)
                |> Collage.move ( x, y )
            ]

        ( Just blue, Just orange ) ->
            [ Collage.oval 10 50
                |> Collage.filled (portalColor Blue)
                |> Collage.move ( blue.x, blue.y )
            , Collage.oval 10 50
                |> Collage.filled (portalColor Orange)
                |> Collage.move ( orange.x, orange.y )
            ]


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.moves MoveMouse
        , Mouse.clicks (\_ -> Click)
        , Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        , AnimationFrame.diffs Tick
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
