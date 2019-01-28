module Main exposing (main)

import Browser exposing (Document)
import Color
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events
import Icons
import Map exposing (Colour(..), Floor(..), Room)
import Selectize
import Set exposing (Set)
import TypedSvg exposing (path, svg)
import TypedSvg.Attributes exposing (class, d, fill, height, stroke, strokeLinecap, strokeLinejoin, strokeWidth)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Events exposing (onMouseLeave, onMouseOver)
import TypedSvg.Types exposing (Fill(..), StrokeLinecap(..), StrokeLinejoin(..), px)


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
    , statsRoom : Maybe Room
    , colourFilter : Set String
    , bookableFilter : ( Bool, Bool )
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
      , statsRoom = Nothing
      , colourFilter = colours
      , bookableFilter = bookableFilter
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

                        "Not Bookable" ->
                            ( Tuple.first model.bookableFilter, checked )

                        _ ->
                            model.bookableFilter
            in
            ( { model | roomMenu = filteredMenu model.colourFilter newBookable, bookableFilter = newBookable }, Cmd.none )

        MouseIn room ->
            ( { model | statsRoom = Just room }, Cmd.none )

        MouseOut ->
            ( { model | statsRoom = Nothing }, Cmd.none )


andDo : Cmd msg -> ( model, Cmd msg ) -> ( model, Cmd msg )
andDo cmd ( model, cmds ) =
    ( model
    , Cmd.batch [ cmd, cmds ]
    )



---- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



---- VIEW


view : Model -> Document Msg
view model =
    { title = "Src Floorplan"
    , body =
        [ Html.div
            [ Attributes.style "display" "flex"
            , Attributes.style "flex-flow" "column"
            ]
            [ Html.div
                [ Attributes.class "container" ]
                [ Html.div
                    [ Attributes.style "width" "30rem" ]
                    [ Selectize.view
                        viewConfig
                        model.selected
                        model.roomMenu
                        |> Html.map RoomMenu
                    ]
                ]
            ]
        , Html.div [] <| floors model.floor
        , Html.div [ Attributes.class "darkbg" ] <| coloursSelect model.colourFilter
        , Html.div [] <| bookableSelect model.bookableFilter
        , mapShow model.colourFilter model.bookableFilter model.floor model.selected
        , Html.div [] [ Html.text (Maybe.map (\r -> r.label) model.statsRoom |> Maybe.withDefault "") ]
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


mapShow : Set String -> ( Bool, Bool ) -> Floor -> Maybe Room -> Html Msg
mapShow colours bookableFilter floor selected =
    let
        room =
            case selected of
                Just value ->
                    Map.showRoom value

                Nothing ->
                    path [] []

        ( box, floorPath, labels ) =
            Map.floorData floor
    in
    svg
        [ class [ "map" ]
        , box
        , height (px 800)
        ]
        (path [ fill FillNone, stroke Color.black, strokeLinecap StrokeLinecapRound, strokeLinejoin StrokeLinejoinRound, strokeWidth (px 1), d floorPath ] []
            :: floorHighlights colours bookableFilter floor
            ++ [ room
               , labels
               ]
        )


floorHighlights : Set String -> ( Bool, Bool ) -> Floor -> List (Svg Msg)
floorHighlights colours bookableFilter floor =
    Map.filterFloor colours bookableFilter floor Map.building
        |> List.map (\room -> path [ fill <| Map.paint room.colour False, d room.path, onMouseOver (MouseIn room), onMouseLeave MouseOut ] [])



---- DATA


bookableSelect : ( Bool, Bool ) -> List (Html Msg)
bookableSelect ( bookable, notBookable ) =
    [ ( bookable, "Bookable" ), ( notBookable, "Not Bookable" ) ]
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
    [ Red, Blue, Green, Yellow ]
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
