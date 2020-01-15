module Icons exposing (camera, checkCircle, circle, projector, screen, speaker)

import Html exposing (Html)
import TypedSvg exposing (path, polyline, svg)
import TypedSvg.Attributes exposing (class, cx, cy, d, opacity, points, r, viewBox)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Opacity(..), px)


svgIcon : String -> List (Svg msg) -> Html msg
svgIcon className =
    svg
        [ class [ "icon", className ]
        , viewBox 0 0 24 24
        ]


svgMedia : Float -> String -> List (Svg msg) -> Html msg
svgMedia x className =
    svg
        [ class [ "media", className ]
        , viewBox 0 0 x 512
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


projector : Html msg
projector =
    svgMedia 648
        "projector"
        [ path [ opacity (Opacity 0.4), d "M592 192h-95.41C543.47 215.77 576 263.93 576 320c0 61.88-39.44 114.31-94.34 134.64L493 499.88A16 16 0 0 0 508.49 512h39A16 16 0 0 0 563 499.88L576 448h16a48 48 0 0 0 48-48V240a48 48 0 0 0-48-48zm-224.59 0H48a48 48 0 0 0-48 48v160a48 48 0 0 0 48 48h16l13 51.88A16 16 0 0 0 92.49 512h39A16 16 0 0 0 147 499.88L160 448h207.41C320.53 424.23 288 376.07 288 320s32.53-104.23 79.41-128zM96 352a32 32 0 1 1 32-32 32 32 0 0 1-32 32zm96 0a32 32 0 1 1 32-32 32 32 0 0 1-32 32zm325.66-218.35a16 16 0 0 0 22.62 0l67.88-67.88a16 16 0 0 0 0-22.63l-11.32-11.31a16 16 0 0 0-22.62 0l-67.88 67.89a16 16 0 0 0 0 22.62zM440 0h-16a16 16 0 0 0-16 16v96a16 16 0 0 0 16 16h16a16 16 0 0 0 16-16V16a16 16 0 0 0-16-16zM323.72 133.65a16 16 0 0 0 22.62 0l11.32-11.31a16 16 0 0 0 0-22.62l-67.88-67.89a16 16 0 0 0-22.62 0l-11.32 11.31a16 16 0 0 0 0 22.63z" ] []
        , path [ d "M96 288a32 32 0 1 0 32 32 32 32 0 0 0-32-32zm336-112a144 144 0 1 0 144 144 144 144 0 0 0-144-144zm0 240a96 96 0 1 1 96-96 96.14 96.14 0 0 1-96 96z" ] []
        ]


screen : Html msg
screen =
    svgMedia 640
        "screen"
        [ path [ opacity (Opacity 0.4), d "M592 0H48A48 48 0 0 0 0 48v320a48 48 0 0 0 48 48h240v32H112a16 16 0 0 0-16 16v32a16 16 0 0 0 16 16h416a16 16 0 0 0 16-16v-32a16 16 0 0 0-16-16H352v-32h240a48 48 0 0 0 48-48V48a48 48 0 0 0-48-48zm-16 352H64V64h512z" ] []
        , path [ d "M576 352H64V64h512z" ] []
        ]


speaker : Html msg
speaker =
    svgMedia 384
        "speaker"
        [ path [ opacity (Opacity 0.4), d "M336 0H48A48 48 0 0 0 0 48v416a48 48 0 0 0 48 48h288a48 48 0 0 0 48-48V48a48 48 0 0 0-48-48zM192 64a48 48 0 1 1-48 48 48 48 0 0 1 48-48zm0 384a112 112 0 1 1 112-112 112 112 0 0 1-112 112z" ] []
        , path [ d "M192 224a112 112 0 1 0 112 112 112 112 0 0 0-112-112zm0 176a64 64 0 1 1 64-64 64 64 0 0 1-64 64zm0-240a48 48 0 1 0-48-48 48 48 0 0 0 48 48z" ] []
        ]


camera : Html msg
camera =
    svgMedia 448
        "camera"
        [ path [ opacity (Opacity 0.4), d "M224 96a128 128 0 1 0 128 128A128 128 0 0 0 224 96zm0 80a48.05 48.05 0 0 0-48 48 16 16 0 0 1-32 0 80.09 80.09 0 0 1 80-80 16 16 0 1 1 0 32z" ] []
        , path [ d "M401 438.6l-49.19-30.75C409.88 367.39 448 300.19 448 224 448 100.29 347.71 0 224 0S0 100.29 0 224c0 76.19 38.12 143.39 96.23 183.85L47 438.6a32 32 0 0 0-15 27.14V480a32 32 0 0 0 32 32h320a32 32 0 0 0 32-32v-14.26a32 32 0 0 0-15-27.14zM224 384a160 160 0 1 1 160-160 160 160 0 0 1-160 160z" ] []
        ]
