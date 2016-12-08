module Math exposing (..)

import Basics exposing (atan2, atan, sin, cos, pi)


type alias Point =
    { x : Float
    , y : Float
    }


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


vector : Float -> Float -> Point
vector x y =
    { x = x
    , y = y
    }


getMeanAngle : List Float -> Float
getMeanAngle list =
    let
        vec =
            List.foldr mean (vector 0.0 0.0) list
    in
        atan2 vec.y vec.x


mean : Float -> Point -> Point
mean direction vec =
    vector
        (vec.x + (cos direction))
        (vec.y + (sin direction))


getDistance : Point -> Point -> Float
getDistance start end =
    sqrt ((end.x - start.x) ^ 2 + (end.y - start.y) ^ 2)


vectorAdd : Point -> Point -> Point
vectorAdd a b =
    vector (a.x + b.x) (a.y + b.y)


vectorSum : List Point -> Point
vectorSum points =
    List.foldr vectorAdd (vector 0.0 0.0) points


getAverageCenter : List Point -> Point
getAverageCenter points =
    let
        sumPos =
            vectorSum points
    in
        vector
            (sumPos.x / toFloat (List.length points))
            (sumPos.y / toFloat (List.length points))


toDeg : Float -> Float
toDeg rad =
    rad * 180 / pi


getCohesion : Point -> Point -> Float
getCohesion position target =
    let
        dx =
            target.x - position.x

        dy =
            target.y - position.y
    in
        (atan2 dy dx)
