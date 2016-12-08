module Art exposing (..)

import Collage exposing (..)


triangle : Float -> Shape
triangle size =
    polygon
        [ ( -size / 2, size / 2 )
        , ( size / 2, size / 2 )
        , ( 0, -size / 2 )
        ]
