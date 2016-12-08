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
    , cohesion = direction
    , alignment = direction
    , position = vector x y
    , radius = 100.0
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick flock dt ->
            ( updatePosition (updateDirection flock model)
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


follow : Point -> Float -> Model -> Model
follow meanPosition meanDirection model =
    let
        -- -- COHESION
        cohesion =
            getCohesion model.position meanPosition

        alignment =
            wrapAngle meanDirection
    in
        { model
            | cohesion = cohesion
            , alignment = alignment
        }


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


updatePosition : Model -> Model
updatePosition model =
    let
        cohesion =
            wrapAngle (model.cohesion - model.direction)

        -- Debug.log "cohesion" (wrapAngle (model.cohesion - model.direction))
        alignment =
            wrapAngle (model.alignment - model.direction)

        -- Debug.log "cohesion" (model.alignment - model.direction)
        direction =
            -- modAngle
            (model.direction
                + (alignment / 50)
                + (cohesion / 50)
            )

        vx =
            model.velocity * cos (direction)

        vy =
            model.velocity * sin (direction)

        position =
            vector
                (model.position.x + vx)
                (model.position.y + vy)
    in
        { model
            | position = position
            , direction = direction
        }


inSight : Model -> Model -> Bool
inSight model target =
    model.radius > getDistance model.position target.position


getFamilly : Model -> List Model -> List Model
getFamilly model flock =
    List.filter (inSight model) flock


tile : Model -> Form
tile model =
    group
        [ filled
            red
            (circle model.radius)
            |> alpha 0.05
        , filled
            red
            (triangle 10)
            |> rotate model.direction
            |> rotate (pi / 2)
        , filled
            red
            (rect 10 1)
            |> rotate model.alignment
        , group
            [ (filled
                blue
                (rect 30 1)
                |> move ( 10, 0 )
              )
            ]
            |> rotate model.cohesion
        ]
        |> move ( model.position.x, model.position.y )
        |> move ( -400, -400 )


view : Model -> Form
view model =
    tile model


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
