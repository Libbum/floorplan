module Main exposing (main)

import Browser exposing (Document)
import Browser.Events
import Color
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events
import Icons
import Json.Decode as Decode exposing (Decoder)
import Map exposing (Colour(..), Floor(..), Room)
import Selectize
import Set exposing (Set)
import TypedSvg exposing (circle, g, path, svg, text_)
import TypedSvg.Attributes exposing (alignmentBaseline, class, cx, cy, d, fill, fontFamily, fontSize, fontWeight, height, r, stroke, strokeLinecap, strokeLinejoin, strokeWidth, textAnchor, transform, width, x, y)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Events exposing (onMouseLeave, onMouseOver)
import TypedSvg.Types exposing (AlignmentBaseline(..), AnchorAlignment(..), Fill(..), FontWeight(..), StrokeLinecap(..), StrokeLinejoin(..), Transform(..), px)


main : Program {} Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



---- MODEL


type alias Model =
    { selected : Maybe Room
    , roomMenu : Selectize.State Room
    , floor : Floor
    , hoverRoom : Maybe Room
    , colourFilter : Set String
    , bookableFilter : ( Bool, Bool )
    , secret : List Keyboard
    , unlocked : Bool
    }


init : {} -> ( Model, Cmd Msg )
init _ =
    let
        colours =
            Set.fromList [ "Red", "Green", "Blue", "Yellow" ]

        bookableFilter =
            ( True, True )
    in
    ( { selected = Nothing
      , roomMenu = filteredMenu colours bookableFilter
      , floor = Two
      , hoverRoom = Nothing
      , colourFilter = colours
      , bookableFilter = bookableFilter
      , secret = []
      , unlocked = False
      }
    , Cmd.none
    )



---- UPDATE


type Msg
    = RoomMenu (Selectize.Msg Room)
    | SelectRoom (Maybe Room)
    | ToggleFloor Floor
    | FilterChange String Bool
    | BookableChange String Bool
    | MouseIn Room
    | MouseOut
    | KeyPress Keyboard
    | Lock Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RoomMenu selectizeMsg ->
            let
                ( newMenu, menuCmd, maybeMsg ) =
                    Selectize.update SelectRoom
                        model.selected
                        model.roomMenu
                        selectizeMsg

                newModel =
                    { model | roomMenu = newMenu }

                cmd =
                    menuCmd |> Cmd.map RoomMenu
            in
            case maybeMsg of
                Just nextMsg ->
                    update nextMsg newModel
                        |> andDo cmd

                Nothing ->
                    ( newModel, cmd )

        SelectRoom newSelection ->
            let
                newFloor =
                    case newSelection of
                        Just room ->
                            room.label |> String.slice 0 1 |> String.toInt |> intToFloor model.floor

                        Nothing ->
                            model.floor
            in
            ( { model | floor = newFloor, selected = newSelection }, Cmd.none )

        ToggleFloor floor ->
            ( { model | floor = floor, selected = Nothing }, Cmd.none )

        FilterChange colour checked ->
            let
                newFilter =
                    if checked then
                        Set.insert colour model.colourFilter

                    else
                        Set.remove colour model.colourFilter

                newSelection =
                    Maybe.andThen
                        (\r ->
                            if Map.colourToString r.colour == colour then
                                Nothing

                            else
                                Just r
                        )
                        model.selected
            in
            ( { model | colourFilter = newFilter, roomMenu = filteredMenu newFilter model.bookableFilter, selected = newSelection }, Cmd.none )

        BookableChange label checked ->
            let
                newBookable =
                    case label of
                        "Bookable" ->
                            ( checked, Tuple.second model.bookableFilter )

                        "Not\u{2007}Bookable" ->
                            ( Tuple.first model.bookableFilter, checked )

                        _ ->
                            model.bookableFilter

                newSelection =
                    Maybe.andThen
                        (\r ->
                            if (label == "Bookable" && r.bookable) || (label == "Not Bookable" && not r.bookable) then
                                Nothing

                            else
                                Just r
                        )
                        model.selected
            in
            ( { model | roomMenu = filteredMenu model.colourFilter newBookable, bookableFilter = newBookable, selected = newSelection }, Cmd.none )

        MouseIn room ->
            ( { model | hoverRoom = Just room }, Cmd.none )

        MouseOut ->
            ( { model | hoverRoom = Nothing }, Cmd.none )

        KeyPress key ->
            let
                newSecret =
                    key :: List.take 7 model.secret
            in
            ( { model | secret = newSecret, unlocked = testSecret newSecret }, Cmd.none )

        Lock button ->
            let
                setLock =
                    if button then
                        False

                    else
                        model.unlocked
            in
            ( { model | unlocked = setLock }, Cmd.none )


andDo : Cmd msg -> ( model, Cmd msg ) -> ( model, Cmd msg )
andDo cmd ( model, cmds ) =
    ( model
    , Cmd.batch [ cmd, cmds ]
    )



---- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onKeyDown (Decode.map KeyPress keyDecoder)


type Keyboard
    = A
    | N
    | D
    | R
    | H
    | E
    | M
    | Other


keyDecoder : Decoder Keyboard
keyDecoder =
    Decode.map toKeyboard (Decode.field "key" Decode.string)


toKeyboard : String -> Keyboard
toKeyboard key =
    case key of
        "a" ->
            A

        "n" ->
            N

        "d" ->
            D

        "r" ->
            R

        "h" ->
            H

        "e" ->
            E

        "m" ->
            M

        _ ->
            Other


testSecret : List Keyboard -> Bool
testSecret secret =
    let
        challenge =
            [ M, E, H, A, R, D, N, A ]
    in
    secret == challenge



---- VIEW


view : Model -> Document Msg
view model =
    { title = "Src Floorplan"
    , body =
        if model.unlocked then
            [ Html.div
                [ Attributes.class "container" ]
                [ Html.button [ Attributes.class "home", Html.Events.onClick (Lock True) ] [ Html.text "Back to work :(" ] ]
            , mapShow model.colourFilter model.bookableFilter AndraHem model.selected model.hoverRoom
            ]

        else
            [ Html.div
                [ Attributes.class "container" ]
                [ Html.div [ Attributes.class "search" ]
                    [ Selectize.view
                        viewConfig
                        model.selected
                        model.roomMenu
                        |> Html.map RoomMenu
                    ]
                , Html.div [] <| floors model.floor
                , Html.div [] <| coloursSelect model.colourFilter
                , Html.div [] <| bookableSelect model.bookableFilter
                ]
            , mapShow model.colourFilter model.bookableFilter model.floor model.selected model.hoverRoom
            ]
    }



---- CONFIGURATION


filteredMenu : Set String -> ( Bool, Bool ) -> Selectize.State Room
filteredMenu colours bookableFilter =
    Selectize.closed
        "textfield-menu"
        (\room -> room.label)
        (roomsSelect colours bookableFilter Map.building)


viewConfig : Selectize.ViewConfig Room
viewConfig =
    Selectize.viewConfig
        { container = []
        , menu =
            [ Attributes.class "selectize__menu" ]
        , ul =
            [ Attributes.class "selectize__list" ]
        , entry =
            \tree mouseFocused keyboardFocused ->
                { attributes =
                    [ Attributes.class "selectize__item"
                    , Attributes.classList
                        [ ( "selectize__item--mouse-selected"
                          , mouseFocused
                          )
                        , ( "selectize__item--key-selected"
                          , keyboardFocused
                          )
                        ]
                    ]
                , children =
                    [ Html.text tree.label ]
                }
        , divider =
            \title ->
                { attributes =
                    [ Attributes.class "selectize__divider" ]
                , children =
                    [ Html.text title ]
                }
        , input = roomSelector
        }


roomSelector : Selectize.Input Room
roomSelector =
    Selectize.autocomplete <|
        { attrs =
            \sthSelected open ->
                [ Attributes.class "selectize__textfield"
                , Attributes.classList
                    [ ( "selectize__textfield--selection", sthSelected )
                    , ( "selectize__textfield--no-selection", not sthSelected )
                    , ( "selectize__textfield--menu-open", open )
                    ]
                ]
        , toggleButton = toggleButton
        , clearButton = clearButton
        , placeholder = "Search for a Room"
        }


toggleButton : Maybe (Bool -> Html Never)
toggleButton =
    Just <|
        \open ->
            Html.div
                [ Attributes.class "selectize__menu-toggle"
                , Attributes.classList
                    [ ( "selectize__menu-toggle--menu-open", open ) ]
                ]
                [ Html.i
                    [ Attributes.class "material-icons"
                    , Attributes.class "selectize__icon"
                    ]
                    [ if open then
                        Html.text "arrow_drop_up"

                      else
                        Html.text "arrow_drop_down"
                    ]
                ]


clearButton : Maybe (Html Never)
clearButton =
    Just <|
        Html.div
            [ Attributes.class "selectize__menu-toggle" ]
            [ Html.i
                [ Attributes.class "material-icons"
                , Attributes.class "selectize__icon"
                ]
                [ Html.text "clear" ]
            ]



--- Map


mapShow : Set String -> ( Bool, Bool ) -> Floor -> Maybe Room -> Maybe Room -> Html Msg
mapShow colours bookableFilter floor selected hover =
    let
        room =
            case selected of
                Just value ->
                    Map.showRoom value

                Nothing ->
                    []

        floorData =
            Map.floorData floor

        showRoom =
            case selected of
                Just _ ->
                    selected

                Nothing ->
                    hover
    in
    svg
        [ class [ "map" ]
        , floorData.box
        ]
        (path [ fill FillNone, stroke Color.black, strokeLinecap StrokeLinecapRound, strokeLinejoin StrokeLinejoinRound, strokeWidth (px 1), d floorData.floorPath ] []
            :: floorData.roomLabels
            :: floorHighlights colours bookableFilter floor
            ++ legend floorData.legendPosition showRoom
            ++ room
        )


floorHighlights : Set String -> ( Bool, Bool ) -> Floor -> List (Svg Msg)
floorHighlights colours bookableFilter floor =
    Map.filterFloor colours bookableFilter floor Map.building
        |> List.map
            (\room ->
                path [ fill <| Map.paint room.colour False, d room.path, onMouseOver (MouseIn room), onMouseLeave MouseOut ] []
            )


legend : { x : Float, y : Float } -> Maybe Room -> List (Svg Msg)
legend legendPosition room =
    let
        lx =
            legendPosition.x

        ly =
            legendPosition.y

        displayRoom =
            case room of
                Just rm ->
                    roomDetails ( lx + 300, ly ) rm

                Nothing ->
                    path [] []
    in
    [ circle [ cx (px <| lx + 25), cy (px <| ly + 25), r (px 20), fill (Fill <| Color.rgba 0.9373 0.4431 0.451 0.3) ] []
    , circle [ cx (px <| lx + 25), cy (px <| ly + 75), r (px 20), fill (Fill <| Color.rgba 1 1 0 0.3) ] []
    , circle [ cx (px <| lx + 25), cy (px <| ly + 125), r (px 20), fill (Fill <| Color.rgba 0 0.439 0.753 0.3) ] []
    , circle [ cx (px <| lx + 25), cy (px <| ly + 175), r (px 20), fill (Fill <| Color.rgba 0 0.69 0.314 0.3) ] []
    , g [ fontFamily [ "Arial", "Helvetica", "sans-serif" ], fontSize (px 16) ]
        [ text_ [ x (px <| lx + 50), y (px <| ly + 29.5) ] [ text "Collaborative work, deep focus" ]
        , text_ [ x (px <| lx + 50), y (px <| ly + 79.5) ] [ text "Individual work, deep focus" ]
        , text_ [ x (px <| lx + 50), y (px <| ly + 129.5) ] [ text "Collaborative work, medium focus" ]
        , text_ [ x (px <| lx + 50), y (px <| ly + 179.5) ] [ text "Individual work, medium focus" ]
        , displayRoom
        ]
    ]


roomDetails : ( Float, Float ) -> Room -> Svg msg
roomDetails ( rx, ry ) room =
    g [ transform [ Translate rx ry ], alignmentBaseline AlignmentMiddle, textAnchor AnchorMiddle ]
        [ text_ [ x (px 100), y (px 50), fontWeight FontWeightBold ] [ text <| String.dropRight (String.length room.label - 27) room.label ]
        , text_ [ x (px 100), y (px 70), fontWeight FontWeightBold ] [ text <| String.dropLeft 27 room.label ]
        , text_ [ x (px 100), y (px 100) ]
            [ text
                (if room.bookable then
                    "Bookable"

                 else
                    "Not Bookable"
                )
            ]
        , text_ [ x (px 100), y (px 120) ]
            [ text
                (if room.capacity > 0 then
                    "Capacity: " ++ String.fromInt room.capacity

                 else
                    ""
                )
            ]
        , text_ [ x (px 100), y (px 140) ]
            [ text
                (if room.exception then
                    "This room has exceptions"

                 else
                    ""
                )
            ]
        ]



---- DATA


bookableSelect : ( Bool, Bool ) -> List (Html Msg)
bookableSelect ( bookable, notBookable ) =
    [ ( bookable, "Bookable" ), ( notBookable, "Not\u{2007}Bookable" ) ]
        |> List.map
            (\( type_, label ) ->
                Html.label []
                    [ Html.input
                        [ Attributes.type_ "checkbox"
                        , Html.Events.onCheck (BookableChange label)
                        , Attributes.checked type_
                        ]
                        []
                    , if type_ then
                        Icons.checkCircle

                      else
                        Icons.circle
                    , Html.text label
                    ]
            )


coloursSelect : Set String -> List (Html Msg)
coloursSelect colours =
    [ Red, Yellow, Blue, Green ]
        |> List.map
            (\c ->
                let
                    label =
                        Map.colourToString c

                    isChecked =
                        Set.member label colours
                in
                Html.label [ Attributes.class ("check-" ++ String.toLower label) ]
                    [ Html.input
                        [ Attributes.type_ "checkbox"
                        , Html.Events.onCheck (FilterChange label)
                        , Attributes.checked isChecked
                        ]
                        []
                    , if isChecked then
                        Icons.checkCircle

                      else
                        Icons.circle
                    , Html.text label
                    ]
            )


intToFloor : Floor -> Maybe Int -> Floor
intToFloor current num =
    Maybe.withDefault current <|
        Maybe.andThen
            (\n ->
                case n of
                    1 ->
                        Just One

                    2 ->
                        Just Two

                    3 ->
                        Just Three

                    4 ->
                        Just Four

                    _ ->
                        Nothing
            )
            num


floors : Floor -> List (Html Msg)
floors current =
    [ One, Two, Three, Four ]
        |> List.map
            (\f ->
                Html.label []
                    [ Html.input
                        [ Attributes.type_ "radio"
                        , Html.Events.onClick (ToggleFloor f)
                        , Attributes.checked (current == f)
                        ]
                        []
                    , if current == f then
                        Icons.checkCircle

                      else
                        Icons.circle
                    , Html.text (floorLabel f)
                    ]
            )


floorLabel : Floor -> String
floorLabel current =
    case current of
        One ->
            "Floor 1"

        Two ->
            "Floor 2"

        Three ->
            "Floor 3"

        Four ->
            "Floor 4"

        AndraHem ->
            "Mitt Andra Hem"


roomsSelect : Set String -> ( Bool, Bool ) -> Dict String Room -> List (Selectize.Entry Room)
roomsSelect colours bookableFilter rooms =
    List.concat
        [ [ Selectize.divider "Floor 1" ]
        , Map.filterFloor colours bookableFilter One rooms |> List.map Selectize.entry
        , [ Selectize.divider "Floor 2" ]
        , Map.filterFloor colours bookableFilter Two rooms |> List.map Selectize.entry
        , [ Selectize.divider "Floor 3" ]
        , Map.filterFloor colours bookableFilter Three rooms |> List.map Selectize.entry
        , [ Selectize.divider "Floor 4" ]
        , Map.filterFloor colours bookableFilter Four rooms |> List.map Selectize.entry
        ]
