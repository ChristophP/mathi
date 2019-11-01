module Icons exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)


pie percentage =
    svg [ viewBox "0 0 20 20" ]
        [ circle
            [ r "10"
            , cx "10"
            , cy "10"
            , fill "red"
            ]
            []
        , circle
            [ class "circleTransition"
            , r "5"
            , cx "10"
            , cy "10"
            , fill "transparent"
            , stroke "green"
            , strokeWidth "10"

            -- 31.4 is an approximation of 10 * PI, to obtain the circumference of the cirle.
            , strokeDasharray <| String.concat [ "calc(", String.fromInt percentage, " * 31.4 / 100) 31.4" ]
            , transform "rotate(-90) translate(-20)"
            ]
            []
        ]
