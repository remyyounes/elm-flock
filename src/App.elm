module App exposing (..)

import Basics exposing (pi)
import Random exposing (generate)
import Time exposing (Time)
import AnimationFrame
import Html exposing (Html, text, div, button)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import Bird exposing (..)


type alias Model =
    { flock : List Bird.Model
    }


xDim : Float
xDim =
    800.0


yDim : Float
yDim =
    800.0


birdInit : Float -> Bird.Model
birdInit direction =
    Bird.init (xDim / 2) (yDim / 2) direction


init : ( Model, Cmd Msg )
init =
    ( Model
        []
    , Cmd.none
    )


type Msg
    = NoOp
    | BirdMsg Bird.Msg
    | Add
    | Insert Float
    | Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Add ->
            ( model, Random.generate Insert (Random.float 0.0 (2 * pi)) )

        Insert direction ->
            ( { model | flock = ((birdInit direction) :: model.flock) }, Cmd.none )

        Tick dt ->
            let
                flock =
                    List.map teleport model.flock

                tickedFlock =
                    List.map (tickBird flock dt) flock
            in
                ( { model | flock = tickedFlock }
                , Cmd.none
                )

        _ ->
            ( model, Cmd.none )


wrap : Float -> Float -> Float
wrap max val =
    if val < 0 then
        val + max
    else if val > max then
        val - max
    else
        val


teleport : Bird.Model -> Bird.Model
teleport bird =
    { bird
        | position =
            vector
                (wrap xDim bird.position.x)
                (wrap yDim bird.position.y)
    }


tickBird : List Bird.Model -> Time -> Bird.Model -> Bird.Model
tickBird flock dt model =
    let
        ( birdModel, birdEffect ) =
            Bird.update (Bird.Tick flock dt) model
    in
        birdModel


viewBird : Bird.Model -> Html Msg
viewBird bird =
    Html.map
        BirdMsg
        (Bird.view bird)


canvasStyle =
    style
        [ ( "background", "rgba(10, 10, 10, 0.1)" )
        , ( "position", "absolute" )
        , ( "width", toString xDim ++ "px" )
        , ( "height", toString yDim ++ "px" )
        ]


view : Model -> Html Msg
view model =
    div [ canvasStyle ]
        [ button [ onClick Add ]
            [ text "Add" ]
        , div []
            (List.map viewBird model.flock)
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.diffs Tick
