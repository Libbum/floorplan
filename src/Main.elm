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
    }


init : {} -> ( Model, Cmd Msg )
init _ =
    ( { selected = Nothing
      , roomMenu = defaultMenu
      , floor = Two
      , statsRoom = Nothing
      }
    , Cmd.none
    )



---- UPDATE


type Msg
    = RoomMenu (Selectize.Msg Room)
    | SelectRoom (Maybe Room)
    | ToggleFloor Floor
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
        , mapShow model.floor model.selected
        , Html.div [] [ Html.text (Maybe.map (\r -> r.label) model.statsRoom |> Maybe.withDefault "") ]
        ]
    }



---- CONFIGURATION


defaultMenu : Selectize.State Room
defaultMenu =
    Selectize.closed
        "textfield-menu"
        (\room -> room.label)
        (roomsSelect Map.building)


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


mapShow : Floor -> Maybe Room -> Html Msg
mapShow floor selected =
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
            :: floorHighlights floor
            ++ [ room
               , labels
               ]
        )


floorHighlights : Floor -> List (Svg Msg)
floorHighlights floor =
    Map.filterFloor floor Map.building
        |> List.map (\room -> path [ fill <| Map.paint room.colour False, d room.path, onMouseOver (MouseIn room), onMouseLeave MouseOut ] [])



---- DATA


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


roomsSelect : Dict String Room -> List (Selectize.Entry Room)
roomsSelect rooms =
    List.concat
        [ [ Selectize.divider "Floor 1" ]
        , Map.filterFloor One rooms |> List.map Selectize.entry
        , [ Selectize.divider "Floor 2" ]
        , Map.filterFloor Two rooms |> List.map Selectize.entry
        , [ Selectize.divider "Floor 3" ]
        , Map.filterFloor Three rooms |> List.map Selectize.entry
        , [ Selectize.divider "Floor 4" ]
        , Map.filterFloor Four rooms |> List.map Selectize.entry
        ]
