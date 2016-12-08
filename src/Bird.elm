module Bird exposing (..)

import Debug
import Basics exposing (atan2, atan, sin, cos, pi)
import Time exposing (Time)
import Color exposing (..)
import Element exposing (..)
import Collage exposing (..)
import Math exposing (..)
import Art exposing (..)


type alias Model =
    { velocity : Float
    , direction : Float
    , position : Point
    , cohesion : Float
    , alignment : Float
    , radius : Float
    }


type Msg
    = NoOp
    | Tick (List Model) Time


init : Float -> Float -> Float -> Model
init x y direction =
    { velocity = 1.0
    , direction = direction
    , cohesion = 0
    , alignment = 0
    , position = vector x y
    , radius = 100.0
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick flock dt ->
            ( move (updateDirection flock model)
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


follow : Point -> Float -> Model -> Model
follow meanPosition meanDirection model =
    let
        -- -- COHESION
        cohesion =
            (getCohesion model.position meanPosition) - model.direction

        alignment =
            wrapAngle ((meanDirection) - (model.direction))

        direction =
            -- modAngle
            (model.direction
                + (alignment / 50)
                + (cohesion / 100)
            )
    in
        { model
            | direction = direction
            , cohesion = cohesion
            , alignment = alignment
        }


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


updateDirection : List Model -> Model -> Model
updateDirection flock model =
    let
        familly =
            getFamilly model flock

        meanAngle =
            getMeanAngle (List.map .direction familly)

        meanPosition =
            getAverageCenter (List.map .position familly)
    in
        if List.length familly > 1 then
            follow meanPosition meanAngle model
        else
            model


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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
