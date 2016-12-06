module Bird exposing (..)

import Debug
import Basics exposing (atan2, atan, sin, cos, pi)
import Time exposing (Time)
import Html exposing (Html, text, div)
import Html.Attributes exposing (style)


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
    , radius = 500.0
    }


type Msg
    = NoOp
    | Tick (List Model) Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick flock dt ->
            ( updatePosition flock model
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


getAngleDelta : Float -> Float -> Float
getAngleDelta a b =
    let
        d =
            a - b
    in
        if d > 2 * pi then
            d - 2 * pi
        else
            d


follow : Model -> Model -> Model
follow target model =
    let
        dx =
            model.position.x - target.position.x

        dy =
            model.position.y - target.position.y

        directionDelta =
            getAngleDelta target.direction model.direction

        newDirection =
            model.direction + directionDelta / 100

        direction =
            if newDirection > pi / 2 then
                newDirection - pi
            else if newDirection < pi / -2 then
                newDirection + pi
            else
                newDirection

        distance =
            getDistance model.position target.position

        vx =
            model.velocity * cos (direction)

        vy =
            model.velocity * sin (direction)

        position =
            vector
                (model.position.x + vx)
                (model.position.y + vy)
    in
        if distance < target.radius then
            Debug.log "+++++"
                { model
                    | direction = Debug.log "direction" direction
                    , position = position
                }
        else
            Debug.log "-----" model


inSight model target =
    model.radius > getDistance model.position target.position


getFamilly model flock =
    List.filter (inSight model) flock


updatePosition : List Model -> Model -> Model
updatePosition flock model =
    List.foldr follow model (getFamilly model flock)


birdStyle : Model -> Html.Attribute msg
birdStyle bird =
    style
        [ ( "backgroundColor", "rgb(238, 238, 236)" )
        , ( "position", "absolute" )
        , ( "left", toString bird.position.x ++ "px" )
        , ( "top", toString bird.position.y ++ "px" )
        ]


view : Model -> Html Msg
view model =
    div [ birdStyle model ] [ text (toString model.position.x) ]


getDistance : Point -> Point -> Float
getDistance start end =
    sqrt ((end.x - start.x) ^ 2 + (end.y - start.y) ^ 2)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
