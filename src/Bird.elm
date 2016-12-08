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
    , separation : Float
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
    , separation = direction
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


updateDirection : List Model -> Model -> Model
updateDirection flock model =
    let
        alignmentFamilly =
            (getFamilly 1.0 model flock)

        cohesionFamilly =
            (getFamilly 0.5 model flock)

        separationFamilly =
            (getFamilly 0.15 model flock)

        alignment =
            if List.length alignmentFamilly > 1 then
                getMeanAngle (List.map .direction alignmentFamilly)
            else
                model.direction

        cohesion =
            if List.length cohesionFamilly > 1 then
                getCohesion
                    model.position
                    (getAverageCenter (List.map .position cohesionFamilly))
            else
                model.direction

        separation =
            if List.length separationFamilly > 1 then
                wrapAngle
                    (pi
                        + getCohesion
                            model.position
                            (getAverageCenter (List.map .position separationFamilly))
                    )
            else
                model.direction
    in
        { model
            | cohesion = cohesion
            , alignment = alignment
            , separation = separation
        }


updatePosition : Model -> Model
updatePosition model =
    let
        cohesion =
            wrapAngle (model.cohesion - model.direction)

        -- Debug.log "cohesion" (wrapAngle (model.cohesion - model.direction))
        alignment =
            wrapAngle (model.alignment - model.direction)

        separation =
            wrapAngle (model.separation - model.direction)

        -- Debug.log "cohesion" (model.alignment - model.direction)
        direction =
            -- modAngle
            (model.direction
                + (alignment / 50)
                + (cohesion / 50)
                + (separation / 40)
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


inSight : Float -> Model -> Model -> Bool
inSight range model target =
    model.radius * range > getDistance model.position target.position


getFamilly : Float -> Model -> List Model -> List Model
getFamilly range model flock =
    List.filter (inSight range model) flock


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
          -- , filled
          --     red
          --     (rect 10 1)
          --     |> rotate model.alignment
          -- , group
          --     [ (filled
          --         blue
          --         (rect 30 1)
          --         |> move ( 10, 0 )
          --       )
          --     ]
          --     |> rotate model.cohesion
        ]
        |> move ( model.position.x, model.position.y )
        |> move ( -400, -400 )


view : Model -> Form
view model =
    tile model


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
