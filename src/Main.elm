module Main exposing (..)

import Html exposing (Html)
import Html.Attributes as Attributes
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
import Maybe.Extra as Maybe
import Json.Encode as Encode


-- MODEL


type alias Model =
    { size : Window.Size
    , player : Player
    , activeGun : PortalColor
    , target : Maybe (Point {})
    , bluePortal : Maybe Portal
    , orangePortal : Maybe Portal
    , level : Level
    , audioMuted : Bool
    }


type alias Player =
    Point
        { vx : Float
        , vy : Float
        }


type alias Portal =
    { location : Point {}
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


type Sign
    = Negative
    | Positive
    | None


initModel : Model
initModel =
    { size = Window.Size 0 0
    , player =
        { x = -230
        , y = -240
        , vx = 0
        , vy = 0
        }
    , activeGun = Blue
    , target = Nothing
    , bluePortal = Nothing
    , orangePortal = Nothing
    , level = Level.level 0
    , audioMuted = False
    }


init : ( Model, Cmd Msg )
init =
    ( initModel
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
            model.target
                |> Maybe.andThen (getOrientation model)
                |> Maybe.map2 Portal model.target
    in
        case model.activeGun of
            Blue ->
                { model | bluePortal = portal }

            Orange ->
                { model | orangePortal = portal }


getOrientation : Model -> Point a -> Maybe Orientation
getOrientation model point =
    let
        wallOrientation =
            model.level.walls
                |> List.filter (isOnWall point)
                |> List.head
                |> Maybe.map (.orientation)
    in
        case wallOrientation of
            Just Level.Horizontal ->
                if model.player.y > point.y then
                    Just Up
                else
                    Just Down

            Just Level.Vertical ->
                if model.player.x > point.x then
                    Just Right
                else
                    Just Left

            Nothing ->
                Nothing


isOnWall : Point a -> Level.Wall -> Bool
isOnWall { x, y } wall =
    case wall.orientation of
        Level.Horizontal ->
            (round y == round wall.origin.y)
                && (clamp wall.origin.x (wall.origin.x + wall.length) x == x)

        Level.Vertical ->
            (round x == round wall.origin.x)
                && (clamp wall.origin.y (wall.origin.y + wall.length) y == y)


setTarget : Mouse.Position -> Model -> Model
setTarget { x, y } model =
    let
        mousePos =
            { x = (toFloat (x * 2 - model.size.width)) / 2
            , y = (toFloat (model.size.height - y * 2)) / 2
            }

        target =
            model.level.walls
                |> List.map (wallIntersection ( model.player, mousePos ))
                |> List.foldl
                    (Maybe.foldMap (closestPoint model.player))
                    Nothing
    in
        { model | target = target }


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


wallIntersection : ( Point a, Point b ) -> Level.Wall -> Maybe (Point {})
wallIntersection line wall =
    let
        ( start, end ) =
            line

        wallEnd =
            wall
                |> Level.endPoint
                |> Point.fromPair

        wallLine =
            ( wall.origin, wallEnd )

        intersect =
            intersection line wallLine
    in
        case wall.orientation of
            Level.Vertical ->
                if
                    (clamp wall.origin.y wallEnd.y intersect.y == intersect.y)
                        && (sign (intersect.x - start.x) == sign (end.x - start.x))
                then
                    Just intersect
                else
                    Nothing

            Level.Horizontal ->
                if
                    (clamp wall.origin.x wallEnd.x intersect.x == intersect.x)
                        && (sign (intersect.y - start.y) == sign (end.y - start.y))
                then
                    Just intersect
                else
                    Nothing


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


step : Time -> Model -> Model
step diff model =
    model
        |> gravity diff
        |> checkPortal
        |> move diff
        |> checkCake


gravity : Time -> Model -> Model
gravity diff ({ player } as model) =
    let
        seconds =
            Time.inSeconds diff

        forceGravity =
            -850
    in
        { model
            | player = { player | vy = player.vy + seconds * forceGravity }
        }


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
            (player.x >= (portal.location.x - 10))
                && (player.x <= (portal.location.x))
                && (player.y > (portal.location.y - 25))
                && (player.y < (portal.location.y + 25))

        Right ->
            (player.x <= (portal.location.x + 10))
                && (player.x >= (portal.location.x))
                && (player.y > (portal.location.y - 25))
                && (player.y < (portal.location.y + 25))

        Up ->
            (player.y <= (portal.location.y + 10))
                && (player.y >= (portal.location.y))
                && (player.x > (portal.location.x - 25))
                && (player.x < (portal.location.x + 25))

        Down ->
            (player.y >= (portal.location.y - 10))
                && (player.y <= (portal.location.y))
                && (player.x > (portal.location.x - 25))
                && (player.x < (portal.location.x + 25))


move : Time -> Model -> Model
move diff ({ player, level } as model) =
    let
        seconds =
            Time.inSeconds diff

        movedPlayer =
            List.foldl
                (checkWall player.x player.y)
                { player | x = player.x + player.vx * seconds, y = player.y + player.vy * seconds }
                level.walls
    in
        { model | player = movedPlayer }


checkWall : Float -> Float -> Level.Wall -> Player -> Player
checkWall oldX oldY wall player =
    case wall.orientation of
        Level.Vertical ->
            if
                (player.vx < 0)
                    && (oldX >= wall.origin.x + 10)
                    && (player.x < wall.origin.x + 10)
                    && (player.y >= wall.origin.y)
                    && (player.y <= wall.origin.y + wall.length)
            then
                { player | x = wall.origin.x + 10, vx = 0 }
            else if
                (player.vx > 0)
                    && (oldX <= wall.origin.x - 10)
                    && (player.x > wall.origin.x - 10)
                    && (player.y >= wall.origin.y)
                    && (player.y <= wall.origin.y + wall.length)
            then
                { player | x = wall.origin.x - 10, vx = 0 }
            else
                player

        Level.Horizontal ->
            if
                (player.vy < 0)
                    && (oldY >= wall.origin.y + 10)
                    && (player.y < wall.origin.y + 10)
                    && (player.x >= wall.origin.x)
                    && (player.x <= wall.origin.x + wall.length)
            then
                { player | y = wall.origin.y + 10, vy = 0 }
            else if
                (player.vy > 0)
                    && (oldY <= wall.origin.y - 10)
                    && (player.y > wall.origin.y - 10)
                    && (player.x >= wall.origin.x)
                    && (player.x <= wall.origin.x + wall.length)
            then
                { player | y = wall.origin.y - 10, vy = 0 }
            else
                player


checkCake : Model -> Model
checkCake model =
    if
        (model.player.x - 10 <= model.level.cake.x + 15)
            && (model.player.x - 10 >= model.level.cake.x - 15)
            && (model.player.y - 10 <= model.level.cake.y + 15)
            && (model.player.y - 10 >= model.level.cake.y - 15)
    then
        { initModel
            | level = Level.level (model.level.index + 1)
            , size = model.size
            , audioMuted = model.audioMuted
        }
    else
        model


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

        Press 83 ->
            { model | audioMuted = not model.audioMuted }

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
        Debug.log "enterPortal" { player | x = x, y = y }


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
    Html.div []
        [ board model
            |> Collage.collage
                model.size.width
                model.size.height
            |> Element.toHtml
        , Html.audio
            [ Attributes.src "./media/still-alive.wav"
            , Attributes.autoplay True
            , Attributes.loop True
            , Attributes.property "muted" (Encode.bool model.audioMuted)
            ]
            []
        ]


board : Model -> List Collage.Form
board { player, target, bluePortal, orangePortal, activeGun, level } =
    Level.view level
        ++ [ characterSprite player
           , target
                |> Maybe.map Point.toPair
                |> Maybe.map (Collage.segment ( player.x, player.y ))
                |> Maybe.map (Collage.traced (Collage.dashed (portalColor activeGun)))
                |> Maybe.withDefault (Collage.toForm Element.empty)
           , bluePortal
                |> Maybe.map (portal Blue)
                |> Maybe.withDefault (Collage.toForm Element.empty)
           , orangePortal
                |> Maybe.map (portal Orange)
                |> Maybe.withDefault (Collage.toForm Element.empty)
           ]


characterSprite : Player -> Collage.Form
characterSprite player =
    if player.vx == 0 then
        Level.image 50 50 "img/stand-right.png" player
    else if player.vx < 0 then
        Level.image 50 50 "img/walk-left.gif" player
    else
        Level.image 50 50 "img/walk-right.gif" player


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
