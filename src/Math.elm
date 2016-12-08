module Math exposing (..)

import Basics exposing (atan2, atan, sin, cos, pi)


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
