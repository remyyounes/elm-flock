module Bird exposing (..)

import Debug
import Basics exposing (atan2, atan, sin, cos, pi)
import Time exposing (Time)
import Html exposing (Html, text, div)
import Html.Attributes exposing (style)
import Color exposing (..)
import Element exposing (..)
import Collage exposing (..)


type alias Point =
    { x : Float
    , y : Float
    }


vector : Float -> Float -> Point
vector x y =
    { x = x
    , y = y
    }


type alias Model =
    { velocity : Float
    , direction : Float
    , position : Point
    , radius : Float
    }


init : Float -> Float -> Float -> Model
init x y direction =
    { velocity = 1.0
    , direction = direction
    , position = vector x y
    , radius = 100.0
    }


type Msg
    = NoOp
    | Tick (List Model) Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick flock dt ->
            ( move (updateDirection flock model)
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


normalizeAngle : Float -> Float
normalizeAngle a =
    if a < 0 then
        a + (2 * pi)
    else
        a


wrapAngle : Float -> Float
wrapAngle a =
    if a > pi then
        -2 * pi + a
    else
        a


modAngle : Float -> Float
modAngle a =
    if a > 2 * pi then
        a - 2 * pi
    else if a < 0 * pi then
        pi - a
    else
        a


follow : Point -> Float -> Model -> Model
follow meanPosition meanDirection model =
    let
        -- -- COHESION
        dx =
            meanPosition.x - model.position.x

        dy =
            meanPosition.y - model.position.y

        cohesionAngle =
            ((atan2 dy dx) - model.direction)

        alignmentAngle =
            wrapAngle ((meanDirection) - (model.direction))

        -- debugAngle =
        --     Debug.log "CohesionAngle" cohesionAngle
        -- debugAngle2 =
        --     Debug.log "wrappedCohesionAngle" (toDeg cohesionAngle)
        -- ADD ANGLE
        -- + cohesionAngle / 100
        direction =
            -- modAngle
            (model.direction
                + (alignmentAngle / 50)
                + (cohesionAngle / 100)
            )
    in
        { model | direction = direction }


move : Model -> Model
move model =
    let
        vx =
            model.velocity * cos (model.direction)

        vy =
            model.velocity * sin (model.direction)

        position =
            vector
                (model.position.x + vx)
                (model.position.y + vy)
    in
        { model | position = position }


inSight : Model -> Model -> Bool
inSight model target =
    model.radius > getDistance model.position target.position


getFamilly : Model -> List Model -> List Model
getFamilly model flock =
    List.filter (inSight model) flock


mean bird vec =
    vector
        (vec.x + (cos bird.direction))
        (vec.y + (sin bird.direction))


getMeanAngle : List Model -> Float
getMeanAngle flock =
    let
        vec =
            List.foldr mean (vector 0.0 0.0) flock
    in
        atan2 vec.y vec.x


vectorAdd : Point -> Point -> Point
vectorAdd a b =
    vector (a.x + b.x) (a.y + b.y)


vectorSum : List Point -> Point
vectorSum points =
    List.foldr vectorAdd (vector 0.0 0.0) points


getCenterFlock : List Model -> Point
getCenterFlock flock =
    let
        sumPos =
            vectorSum (List.map .position flock)
    in
        vector
            (sumPos.x / toFloat (List.length flock))
            (sumPos.y / toFloat (List.length flock))


updateDirection : List Model -> Model -> Model
updateDirection flock model =
    let
        familly =
            getFamilly model flock

        meanAngle =
            getMeanAngle familly

        -- a =
        --     Debug.log "dir" (toDeg model.direction)
        -- b =
        --     Debug.log "meanDir" (toDeg meanAngle)
        meanPosition =
            getCenterFlock familly
    in
        if List.length familly > 1 then
            follow meanPosition meanAngle model
        else
            model


toDeg : Float -> Float
toDeg rad =
    rad * 180 / pi


birdStyle : Model -> Html.Attribute msg
birdStyle bird =
    style
        [ ( "border", "1px solid rgba(10, 10, 10, 1)" )
        , ( "position", "absolute" )
          -- , ( "padding", toString bird.radius ++ "px" )
          -- , ( "margin", toString -bird.radius ++ "px" )
        , ( "border-radius", toString bird.radius ++ "px" )
        , ( "left", toString bird.position.x ++ "px" )
        , ( "top", toString bird.position.y ++ "px" )
        , ( "transform", "rotate(" ++ toString (toDeg bird.direction) ++ "deg)" )
        ]



-- view : Model -> Html Msg
-- view model =
--     div [ birdStyle model ] [ Html.text ">" ]


triangle size =
    polygon
        [ ( -size / 2, size / 2 )
        , ( size / 2, size / 2 )
        , ( 0, -size / 2 )
        ]


tile : Model -> Form
tile model =
    filled red (triangle 10)
        |> Collage.move ( model.position.x, model.position.y )
        |> Collage.move ( -400, -400 )
        |> rotate model.direction
        |> rotate (pi / 2)


view : Model -> Form
view model =
    tile model


getDistance : Point -> Point -> Float
getDistance start end =
    sqrt ((end.x - start.x) ^ 2 + (end.y - start.y) ^ 2)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
