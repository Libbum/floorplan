module Main exposing (main)

import Browser exposing (Document)
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attributes
import Map
import Selectize


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
    { selected : Maybe String
    , roomMenu : Selectize.State String
    , rooms : Dict String Room
    }


init : {} -> ( Model, Cmd Msg )
init _ =
    let
        rooms =
            building
    in
    ( { selected = Nothing
      , roomMenu =
            Selectize.closed
                "textfield-menu"
                identity
                (roomsSelect rooms)
      , rooms = rooms
      }
    , Cmd.none
    )



---- UPDATE


type Msg
    = TextfieldMenuMsg (Selectize.Msg String)
    | SelectTextfieldLicense (Maybe String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TextfieldMenuMsg selectizeMsg ->
            let
                ( newMenu, menuCmd, maybeMsg ) =
                    Selectize.update SelectTextfieldLicense
                        model.selected
                        model.roomMenu
                        selectizeMsg

                newModel =
                    { model | roomMenu = newMenu }

                cmd =
                    menuCmd |> Cmd.map TextfieldMenuMsg
            in
            case maybeMsg of
                Just nextMsg ->
                    update nextMsg newModel
                        |> andDo cmd

                Nothing ->
                    ( newModel, cmd )

        SelectTextfieldLicense newSelection ->
            ( { model | selected = newSelection }, Cmd.none )


andDo : Cmd msg -> ( model, Cmd msg ) -> ( model, Cmd msg )
andDo cmd ( model, cmds ) =
    ( model
    , Cmd.batch [ cmd, cmds ]
    )



---- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
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
                        viewConfigTextfield
                        model.selected
                        model.roomMenu
                        |> Html.map TextfieldMenuMsg
                    ]
                ]
            , Html.text (Maybe.withDefault "" model.selected)
            , Map.floor1
            , Map.room106
            ]
        ]
    }



---- CONFIGURATION


viewConfigTextfield : Selectize.ViewConfig String
viewConfigTextfield =
    viewConfig textfieldSelector


viewConfig : Selectize.Input String -> Selectize.ViewConfig String
viewConfig selector =
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
                    [ Html.text tree ]
                }
        , divider =
            \title ->
                { attributes =
                    [ Attributes.class "selectize__divider" ]
                , children =
                    [ Html.text title ]
                }
        , input = selector
        }


textfieldSelector : Selectize.Input String
textfieldSelector =
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
        , placeholder = "Select a Room"
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



---- DATA


roomsSelect : Dict String Room -> List (Selectize.Entry String)
roomsSelect rooms =
    List.concat
        [ [ Selectize.divider "Floor 1" ]
        , floor 1 rooms |> List.map Selectize.entry
        , [ Selectize.divider "Floor 2" ]
        , floor 2 rooms |> List.map Selectize.entry
        , [ Selectize.divider "Floor 3" ]
        , floor 3 rooms |> List.map Selectize.entry
        , [ Selectize.divider "Floor 4" ]
        , floor 4 rooms |> List.map Selectize.entry
        ]


type alias Room =
    { label : String
    , colour : Colour
    , bookable : Bool
    , exception : Bool
    }


type Colour
    = Red
    | Yellow
    | Green
    | Blue
    | Clear


floor : Int -> Dict String Room -> List String
floor num rooms =
    Dict.filter (\key val -> String.startsWith (String.fromInt num) key) rooms
        |> Dict.values
        |> List.map (\room -> room.label)


building : Dict String Room
building =
    Dict.fromList
        [ ( "106", { label = "106 Meeting Room", colour = Red, bookable = True, exception = False } )
        , ( "108", { label = "108 Archive", colour = Clear, bookable = False, exception = False } )
        , ( "109", { label = "109 Secure Archive", colour = Clear, bookable = False, exception = False } )
        , ( "110", { label = "110 Meditation Room", colour = Clear, bookable = False, exception = False } )
        , ( "206", { label = "206 Telephone Room", colour = Yellow, bookable = False, exception = False } )
        , ( "208B", { label = "208B Telephone Room", colour = Yellow, bookable = False, exception = False } )
        , ( "208", { label = "208 Hall", colour = Blue, bookable = False, exception = False } )
        , ( "209", { label = "209 Group Room", colour = Blue, bookable = False, exception = False } )
        , ( "211", { label = "211 Office", colour = Green, bookable = False, exception = False } )
        , ( "212", { label = "212 Office", colour = Green, bookable = False, exception = False } )
        , ( "213", { label = "213 Office (Admin Room)", colour = Green, bookable = False, exception = True } )
        , ( "214", { label = "214 Office", colour = Green, bookable = False, exception = False } )
        , ( "216", { label = "216 Lobby", colour = Blue, bookable = False, exception = False } )
        , ( "222", { label = "222 Massage/Sick Room", colour = Yellow, bookable = False, exception = False } )
        , ( "226", { label = "226 Meeting Room", colour = Red, bookable = True, exception = False } )
        , ( "237", { label = "237 Meeting Room", colour = Red, bookable = True, exception = False } )
        , ( "238A", { label = "238A Meeting Room", colour = Red, bookable = True, exception = False } )
        , ( "238B", { label = "238B Meeting Room", colour = Red, bookable = True, exception = False } )
        , ( "238C", { label = "238C Group Room", colour = Blue, bookable = False, exception = False } )
        , ( "239", { label = "239 Meeting Room", colour = Red, bookable = True, exception = False } )
        , ( "240", { label = "240 Kitchen", colour = Yellow, bookable = False, exception = False } )
        , ( "248", { label = "248 Meeting Room", colour = Red, bookable = True, exception = False } )
        , ( "249", { label = "249 Group Room", colour = Blue, bookable = False, exception = False } )
        , ( "250", { label = "250 Meeting Room", colour = Red, bookable = True, exception = False } )
        , ( "251", { label = "251 Lecture Room", colour = Red, bookable = True, exception = False } )
        , ( "306", { label = "306 Group Room", colour = Blue, bookable = False, exception = False } )
        , ( "307", { label = "307 Thorsten's Room", colour = Red, bookable = False, exception = True } )
        , ( "308", { label = "308 Group Room", colour = Blue, bookable = False, exception = False } )
        , ( "310", { label = "310 Telephone Room", colour = Red, bookable = True, exception = False } )
        , ( "312", { label = "312 Library", colour = Yellow, bookable = False, exception = False } )
        , ( "313", { label = "313 Meeting Room", colour = Red, bookable = True, exception = False } )
        , ( "314", { label = "314 Office", colour = Green, bookable = False, exception = False } )
        , ( "315", { label = "315 Office", colour = Green, bookable = False, exception = False } )
        , ( "316", { label = "316 Telephone Room", colour = Red, bookable = True, exception = False } )
        , ( "321", { label = "321 Meeting Room", colour = Red, bookable = True, exception = False } )
        , ( "324", { label = "324 Lounge", colour = Blue, bookable = False, exception = False } )
        , ( "324B", { label = "324B Group Room", colour = Green, bookable = False, exception = False } )
        , ( "324C", { label = "324C Office", colour = Green, bookable = False, exception = False } )
        , ( "324D", { label = "324D Office (SRC Leadership Representation Room)", colour = Green, bookable = False, exception = True } )
        , ( "325", { label = "325 Office (Malin's Room)", colour = Yellow, bookable = False, exception = True } )
        , ( "326", { label = "326 Office", colour = Yellow, bookable = False, exception = False } )
        , ( "327", { label = "327 Office", colour = Yellow, bookable = False, exception = False } )
        , ( "328", { label = "328 Office", colour = Yellow, bookable = False, exception = False } )
        , ( "329", { label = "329 Office", colour = Yellow, bookable = False, exception = False } )
        , ( "330", { label = "330 Office", colour = Yellow, bookable = False, exception = False } )
        , ( "331", { label = "331 Office", colour = Yellow, bookable = False, exception = False } )
        , ( "332", { label = "332 Office", colour = Yellow, bookable = False, exception = False } )
        , ( "334", { label = "334 Office", colour = Yellow, bookable = False, exception = False } )
        , ( "342A", { label = "342A Lounge", colour = Blue, bookable = False, exception = False } )
        , ( "342B", { label = "342B Office", colour = Green, bookable = False, exception = False } )
        , ( "342C", { label = "342C Telephone Room", colour = Red, bookable = True, exception = False } )
        , ( "342D", { label = "342D Telephone Room", colour = Red, bookable = True, exception = False } )
        , ( "342E", { label = "342E Office", colour = Green, bookable = False, exception = False } )
        , ( "342F", { label = "342F Group Room", colour = Blue, bookable = False, exception = False } )
        , ( "343", { label = "343 Office", colour = Yellow, bookable = False, exception = False } )
        , ( "344", { label = "344 Office", colour = Yellow, bookable = False, exception = False } )
        , ( "345", { label = "345 Office", colour = Yellow, bookable = False, exception = False } )
        , ( "346", { label = "346 Office", colour = Yellow, bookable = False, exception = False } )
        , ( "347", { label = "347 Office", colour = Yellow, bookable = False, exception = False } )
        , ( "348", { label = "348 Office", colour = Yellow, bookable = False, exception = False } )
        , ( "349", { label = "349 Office", colour = Yellow, bookable = False, exception = False } )
        , ( "350", { label = "350 Office", colour = Yellow, bookable = False, exception = False } )
        , ( "351", { label = "351 Office", colour = Yellow, bookable = False, exception = False } )
        , ( "352", { label = "352 Office", colour = Yellow, bookable = False, exception = False } )
        , ( "355", { label = "355 Meeting Room", colour = Red, bookable = True, exception = False } )
        , ( "406", { label = "406 Office", colour = Yellow, bookable = False, exception = False } )
        , ( "407", { label = "407 Office", colour = Yellow, bookable = False, exception = False } )
        , ( "408", { label = "408 Office", colour = Yellow, bookable = False, exception = False } )
        , ( "409", { label = "409 Office", colour = Yellow, bookable = False, exception = False } )
        , ( "410", { label = "410 Telephone Room", colour = Red, bookable = True, exception = False } )
        , ( "412", { label = "412 Office", colour = Yellow, bookable = False, exception = False } )
        , ( "413", { label = "413 Office", colour = Yellow, bookable = False, exception = False } )
        , ( "414", { label = "414 Office (Calle's Room)", colour = Yellow, bookable = False, exception = True } )
        , ( "418", { label = "418 Telephone Room", colour = Red, bookable = True, exception = True } )
        ]
