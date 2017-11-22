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
import Level exposing (Level)
import Point exposing (Point)


-- MODEL


type alias Model =
    { size : Window.Size
    , player : Player
    , activeGun : PortalColor
    , target : Point
    , bluePortal : Maybe Portal
    , orangePortal : Maybe Portal
    , level : Level
    }


type alias Player =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    }


type alias Portal =
    { location : Point
    , orientation : Orientation
    }


type Orientation
    = Left
    | Up
    | Right
    | Down


type PortalColor
    = Blue
    | Orange


type KeyPress
    = Lift Int
    | Press Int


init : ( Model, Cmd Msg )
init =
    ( { size = Window.Size 0 0
      , player =
          { x = -230
          , y = -240
          , vx = 0
          , vy = 0
          }
      , activeGun = Blue
      , target =
          { x = -230
          , y = -240
          }
      , bluePortal = Nothing
      , orangePortal = Nothing
      , level = Level.level 1
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
            ( onKeyPress (Press code) model, Cmd.none )

        KeyUp code ->
            ( onKeyPress (Lift code) model, Cmd.none )

        Click ->
            ( shootPortal model, Cmd.none )


shootPortal : Model -> Model
shootPortal model =
    let
        portal =
            Just { location = model.target, orientation = getOrientation model.target }
    in
        case model.activeGun of
            Blue ->
                { model | bluePortal = portal }

            Orange ->
                { model | orangePortal = portal }


getOrientation : Point -> Orientation
getOrientation { x, y } =
    if round x == 250 then
        Left
    else if round x == -250 then
        Right
    else if round y == 250 then
        Down
    else
        Up


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

        wallTarget =
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

        target =
            model.level.walls
                |> List.map (wallIntersection ( playerPos, mousePos ))
                |> List.filterMap identity
                |> List.foldl (closestPoint playerPos) wallTarget
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


wallIntersection : ( Point, Point ) -> Level.Wall -> Maybe Point
wallIntersection line wall =
    let
        wallEnd =
            wall
                |> Level.endPoint
                |> uncurry Point

        wallLine =
            ( wall.origin, wallEnd )

        intersect =
            intersection line wallLine
    in
        case wall.orientation of
            Level.Vertical ->
                if clamp wall.origin.y wallEnd.y intersect.y == intersect.y then
                    Just intersect
                else
                    Nothing

            Level.Horizontal ->
                if clamp wall.origin.x wallEnd.x intersect.x == intersect.x then
                    Just intersect
                else
                    Nothing


length : Point -> Point -> Float
length a b =
    sqrt <| (a.x - b.x) ^ 2 + (a.y - b.y) ^ 2


closestPoint : Point -> Point -> Point -> Point
closestPoint toPt pt1 pt2 =
    if length pt1 toPt <= length pt2 toPt then
        pt1
    else
        pt2


step : Time -> Model -> Model
step diff model =
    model
        |> gravity diff
        |> checkPortal
        |> move diff
        |> checkBoundaries


gravity : Time -> Model -> Model
gravity diff ({ player } as model) =
    let
        seconds =
            Time.inSeconds diff

        forceGravity =
            -850
    in
        if player.y <= -240 then
            { model | player = { player | vy = 0 } }
        else
            { model | player = { player | vy = player.vy + seconds * forceGravity } }


checkBoundaries : Model -> Model
checkBoundaries ({ player } as model) =
    if player.x <= -240 && player.vx < 0 then
        { model | player = { player | vx = 0, x = -240 } }
    else if player.x >= 240 && player.vx > 0 then
        { model | player = { player | vx = 0, x = 240 } }
    else if player.y <= -240 && player.vy < 0 then
        { model | player = { player | vy = 0, y = -240 } }
    else if player.y >= 240 && player.vy > 0 then
        { model | player = { player | vy = 0, y = 240 } }
    else
        model


checkPortal : Model -> Model
checkPortal model =
    let
        inBluePortal =
            model.bluePortal
                |> Maybe.map (isInPortal model.player)
                |> Maybe.withDefault False

        inOrangePortal =
            model.orangePortal
                |> Maybe.map (isInPortal model.player)
                |> Maybe.withDefault False

        from =
            if inBluePortal then
                model.bluePortal
            else if inOrangePortal then
                model.orangePortal
            else
                Nothing

        to =
            if inOrangePortal then
                model.bluePortal
            else
                model.orangePortal

        player =
            Maybe.map2 (enterPortal model.player) from to
                |> Maybe.withDefault model.player
    in
        { model | player = player }


isInPortal : Player -> Portal -> Bool
isInPortal player portal =
    case portal.orientation of
        Left ->
            player.x >= (portal.location.x - 10) &&
            player.y > (portal.location.y - 25) &&
            player.y < (portal.location.y + 25)

        Right ->
            player.x <= (portal.location.x + 10) &&
            player.y > (portal.location.y - 25) &&
            player.y < (portal.location.y + 25)

        Up ->
            player.y <= (portal.location.y + 10) &&
            player.x > (portal.location.x - 25) &&
            player.x < (portal.location.x + 25)

        Down ->
            player.y >= (portal.location.y - 10) &&
            player.x > (portal.location.x - 25) &&
            player.x < (portal.location.x + 25)


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
        Press 37 ->
            { model | player = { player | vx = -velocity } }

        Lift 37 ->
            { model | player = { player | vx = 0 } }

        Press 65 ->
            { model | player = { player | vx = -velocity } }

        Lift 65 ->
            { model | player = { player | vx = 0 } }

        Press 39 ->
            { model | player = { player | vx = velocity } }

        Lift 39 ->
            { model | player = { player | vx = 0 } }

        Press 68 ->
            { model | player = { player | vx = velocity } }

        Lift 68 ->
            { model | player = { player | vx = 0 } }

        Press 16 ->
            { model | activeGun = switchGuns activeGun }

        _ ->
            model


switchGuns : PortalColor -> PortalColor
switchGuns activeGun =
    case activeGun of
        Blue ->
            Orange

        Orange ->
            Blue


enterPortal : Player -> Portal -> Portal -> Player
enterPortal player from to =
    let
        offset =
            if isVertical from.orientation then
                player.y - from.location.y
            else
                player.x - from.location.x

        x =
            case to.orientation of
                Left ->
                    to.location.x - 15

                Right ->
                    to.location.x + 15

                Up ->
                    to.location.x + offset

                Down ->
                    to.location.x + offset

        y =
            case to.orientation of
                Left ->
                    to.location.y + offset

                Right ->
                    to.location.y + offset

                Up ->
                    to.location.y + 15

                Down ->
                    to.location.y - 15
    in
        { player | x = x, y = y }


isVertical : Orientation -> Bool
isVertical orientation =
    case orientation of
        Left ->
            True

        Right ->
            True

        Up ->
            False

        Down ->
            False


-- VIEW


view : Model -> Html Msg
view model =
    board model
        |> Collage.collage
            model.size.width
            model.size.height
        |> Element.toHtml



board : Model -> List Collage.Form
board { player, target, bluePortal, orangePortal, activeGun, level } =
    [ Collage.rect 500 500
        |> Collage.outlined (Collage.solid Color.black)
    , Collage.circle 10
        |> Collage.filled Color.gray
        |> Collage.move ( player.x, player.y )
    , Collage.segment
        ( player.x, player.y )
        ( target.x, target.y )
            |> Collage.traced (Collage.dashed (portalColor activeGun))
    , bluePortal
        |> Maybe.map (portal Blue)
        |> Maybe.withDefault (Collage.toForm Element.empty)
    , orangePortal
        |> Maybe.map (portal Orange)
        |> Maybe.withDefault (Collage.toForm Element.empty)
    ]
    ++ (Level.view level)


portalColor : PortalColor -> Color
portalColor portal =
    case portal of
        Blue ->
            Color.blue

        Orange ->
            Color.orange


portal : PortalColor -> Portal -> Collage.Form
portal color { location, orientation } =
    portalShape orientation
        |> Collage.filled (portalColor color)
        |> Collage.move ( location.x, location.y )


portalShape : Orientation -> Collage.Shape
portalShape orientation =
    case orientation of
        Left ->
            Collage.oval 10 50

        Right ->
            Collage.oval 10 50

        Up ->
            Collage.oval 50 10

        Down ->
            Collage.oval 50 10


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
