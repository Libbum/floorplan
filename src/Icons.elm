module Icons exposing (checkCircle, circle)

import Html exposing (Html)
import TypedSvg exposing (path, polyline, svg)
import TypedSvg.Attributes exposing (class, cx, cy, d, points, r, viewBox)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (px)


svgIcon : String -> List (Svg msg) -> Html msg
svgIcon className =
    svg
        [ class [ "icon", className ]
        , viewBox 0 0 24 24
        ]


checkCircle : Html msg
checkCircle =
    svgIcon "check-circle"
        [ path [ d "M22 11.08V12a10 10 0 1 1-5.93-9.14" ] []
        , polyline [ points [ ( 22, 4 ), ( 12, 14.01 ), ( 9, 11.01 ) ] ] []
        ]


circle : Html msg
circle =
    svgIcon "circle"
        [ TypedSvg.circle [ cx (px 12), cy (px 12), r (px 10) ] []
        ]
