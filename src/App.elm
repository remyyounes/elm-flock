module App exposing (..)

import Time exposing (Time)
import AnimationFrame
import Html exposing (Html, text, div)
import Bird exposing (..)


type alias Model =
    { flock : List Bird.Model
    }


init : ( Model, Cmd Msg )
init =
    ( Model
        [ (Bird.init 50.0 50.0 1.5)
        , (Bird.init 50.0 100.0 1.0)
        , (Bird.init 0.0 100.0 0.0)
        ]
    , Cmd.none
    )


type Msg
    = NoOp
    | BirdMsg Bird.Msg
    | Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
                (wrap 500.0 bird.position.x)
                (wrap 500.0 bird.position.y)
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


view : Model -> Html Msg
view model =
    div []
        (List.map viewBird model.flock)


subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.diffs Tick
