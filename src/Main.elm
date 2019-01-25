module Main exposing (main)

import Browser exposing (Document)
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attributes
import Map exposing (Colour(..), Room)
import Selectize
import TypedSvg.Core exposing (Svg)


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
                (\room -> room.label)
                (roomsSelect rooms)
      , rooms = rooms
      }
    , Cmd.none
    )



---- UPDATE


type Msg
    = RoomMenu (Selectize.Msg Room)
    | SelectRoom (Maybe Room)


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
                        viewConfig
                        model.selected
                        model.roomMenu
                        |> Html.map RoomMenu
                    ]
                ]
            , Map.show model.selected
            ]
        ]
    }



---- CONFIGURATION


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


roomsSelect : Dict String Room -> List (Selectize.Entry Room)
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


floor : Int -> Dict String Room -> List Room
floor num rooms =
    Dict.filter (\key val -> String.startsWith (String.fromInt num) key) rooms
        |> Dict.values


building : Dict String Room
building =
    Dict.fromList
        [ ( "106"
          , { label = "106 Basement"
            , colour = Red
            , bookable = True
            , exception = False
            , capacity = 6
            , path = "M227 222h102.6v66.6h-102.6v-66.6z"
            }
          )
        , ( "108"
          , { label = "108 Archive"
            , colour = Clear
            , bookable = False
            , exception = False
            , capacity = 0
            , path = "M192.667 349.667l135.666-.334.667 66.334-136.333.333v-66.333z"
            }
          )
        , ( "109"
          , { label = "109 Secure Archive"
            , colour = Clear
            , bookable = False
            , exception = False
            , capacity = 0
            , path = "M192.85 427.75l119.4-.3v76.6h-100.2l.2-13.3h-19.9l.5-63z"
            }
          )
        , ( "110"
          , { label = "110 Meditation Room"
            , colour = Clear
            , bookable = False
            , exception = False
            , capacity = 0
            , path = "M212.25 513.65l100-.4v111h-131.9v-77.5h24l-.4 42.7h8.1l.2-75.8z"
            }
          )
        , ( "206"
          , { label = "206 Telephone Room"
            , colour = Yellow
            , bookable = True
            , exception = False
            , capacity = 3
            , path = ""
            }
          )
        , ( "208B"
          , { label = "208B Telephone Room"
            , colour = Yellow
            , bookable = False
            , exception = False
            , capacity = 1
            , path = ""
            }
          )
        , ( "208"
          , { label = "208 Hall"
            , colour = Blue
            , bookable = False
            , exception = False
            , capacity = 2
            , path = ""
            }
          )
        , ( "209"
          , { label = "209 Group Room"
            , colour = Blue
            , bookable = False
            , exception = False
            , capacity = 8
            , path = ""
            }
          )
        , ( "211"
          , { label = "211 Office"
            , colour = Green
            , bookable = False
            , exception = False
            , capacity = 3
            , path = ""
            }
          )
        , ( "212"
          , { label = "212 Office"
            , colour = Green
            , bookable = False
            , exception = False
            , capacity = 4
            , path = ""
            }
          )
        , ( "213"
          , { label = "213 Office (Admin Room)"
            , colour = Green
            , bookable = False
            , exception = True
            , capacity = 4
            , path = ""
            }
          )
        , ( "214"
          , { label = "214 Office"
            , colour = Green
            , bookable = False
            , exception = False
            , capacity = 4
            , path = ""
            }
          )
        , ( "216"
          , { label = "216 Lobby"
            , colour = Blue
            , bookable = True
            , exception = False
            , capacity = 30
            , path = ""
            }
          )
        , ( "222"
          , { label = "222 Massage/Sick Room"
            , colour = Yellow
            , bookable = False
            , exception = False
            , capacity = 2
            , path = ""
            }
          )
        , ( "226"
          , { label = "226 Calle's Old Room"
            , colour = Red
            , bookable = True
            , exception = False
            , capacity = 6
            , path = ""
            }
          )
        , ( "237"
          , { label = "237 Big Room in the Corner"
            , colour = Red
            , bookable = True
            , exception = False
            , capacity = 30
            , path = ""
            }
          )
        , ( "238A"
          , { label = "238A IT Support's Old Room"
            , colour = Red
            , bookable = True
            , exception = False
            , capacity = 4
            , path = ""
            }
          )
        , ( "238B"
          , { label = "238B The Smallest"
            , colour = Red
            , bookable = True
            , exception = False
            , capacity = 4
            , path = ""
            }
          )
        , ( "238C"
          , { label = "238C Inbetweeners"
            , colour = Blue
            , bookable = True
            , exception = False
            , capacity = 6
            , path = ""
            }
          )
        , ( "239"
          , { label = "239 Next to Kitchen"
            , colour = Red
            , bookable = True
            , exception = False
            , capacity = 20
            , path = ""
            }
          )
        , ( "240"
          , { label = "240 Kitchen"
            , colour = Yellow
            , bookable = True
            , exception = False
            , capacity = 40
            , path = ""
            }
          )
        , ( "248"
          , { label = "248 Board Room"
            , colour = Red
            , bookable = True
            , exception = False
            , capacity = 12
            , path = ""
            }
          )
        , ( "249"
          , { label = "249 Group Room"
            , colour = Blue
            , bookable = False
            , exception = False
            , capacity = 12
            , path = ""
            }
          )
        , ( "250"
          , { label = "250 Old Communications Lab"
            , colour = Red
            , bookable = True
            , exception = False
            , capacity = 30
            , path = ""
            }
          )
        , ( "251"
          , { label = "251 Master's Lecture"
            , colour = Red
            , bookable = True
            , exception = False
            , capacity = 40
            , path = ""
            }
          )
        , ( "306"
          , { label = "306 Group Room"
            , colour = Blue
            , bookable = False
            , exception = False
            , capacity = 12
            , path = ""
            }
          )
        , ( "307"
          , { label = "307 Thorsten's Room"
            , colour = Red
            , bookable = False
            , exception = True
            , capacity = 1
            , path = ""
            }
          )
        , ( "308"
          , { label = "308 Group Room"
            , colour = Blue
            , bookable = False
            , exception = False
            , capacity = 6
            , path = ""
            }
          )
        , ( "310"
          , { label = "310 Telephone Room"
            , colour = Yellow
            , bookable = False
            , exception = False
            , capacity = 1
            , path = ""
            }
          )
        , ( "312"
          , { label = "312 Library"
            , colour = Yellow
            , bookable = False
            , exception = False
            , capacity = 12
            , path = ""
            }
          )
        , ( "313"
          , { label = "313 Thomas' Old Room"
            , colour = Red
            , bookable = True
            , exception = False
            , capacity = 6
            , path = ""
            }
          )
        , ( "314"
          , { label = "314 Office"
            , colour = Green
            , bookable = False
            , exception = False
            , capacity = 4
            , path = ""
            }
          )
        , ( "315"
          , { label = "315 Office"
            , colour = Green
            , bookable = False
            , exception = False
            , capacity = 4
            , path = ""
            }
          )
        , ( "316"
          , { label = "316 Telephone Room"
            , colour = Red
            , bookable = False
            , exception = False
            , capacity = 4
            , path = ""
            }
          )
        , ( "321"
          , { label = "321 Corner North Attic"
            , colour = Red
            , bookable = True
            , exception = False
            , capacity = 10
            , path = ""
            }
          )
        , ( "324"
          , { label = "324 Lounge"
            , colour = Blue
            , bookable = False
            , exception = False
            , capacity = 6
            , path = ""
            }
          )
        , ( "324B"
          , { label = "324B Group Room"
            , colour = Green
            , bookable = False
            , exception = False
            , capacity = 10
            , path = ""
            }
          )
        , ( "324C"
          , { label = "324C Office"
            , colour = Green
            , bookable = False
            , exception = False
            , capacity = 3
            , path = ""
            }
          )
        , ( "324D"
          , { label = "324D Office (SRC Leadership Representation Room)"
            , colour = Green
            , bookable = False
            , exception = True
            , capacity = 3
            , path = ""
            }
          )
        , ( "325"
          , { label = "325 Office (Malin's Room)"
            , colour = Yellow
            , bookable = False
            , exception = True
            , capacity = 1
            , path = ""
            }
          )
        , ( "326"
          , { label = "326 Office"
            , colour = Yellow
            , bookable = False
            , exception = False
            , capacity = 1
            , path = ""
            }
          )
        , ( "327"
          , { label = "327 Office"
            , colour = Yellow
            , bookable = False
            , exception = False
            , capacity = 1
            , path = ""
            }
          )
        , ( "328"
          , { label = "328 Office (ERC Management)"
            , colour = Yellow
            , bookable = False
            , exception = True
            , capacity = 1
            , path = ""
            }
          )
        , ( "329"
          , { label = "329 Office"
            , colour = Yellow
            , bookable = False
            , exception = False
            , capacity = 1
            , path = ""
            }
          )
        , ( "330"
          , { label = "330 Office"
            , colour = Yellow
            , bookable = False
            , exception = False
            , capacity = 1
            , path = ""
            }
          )
        , ( "331"
          , { label = "331 Office"
            , colour = Yellow
            , bookable = False
            , exception = False
            , capacity = 2
            , path = ""
            }
          )
        , ( "332"
          , { label = "332 Office"
            , colour = Yellow
            , bookable = False
            , exception = False
            , capacity = 2
            , path = ""
            }
          )
        , ( "334"
          , { label = "334 Office"
            , colour = Yellow
            , bookable = False
            , exception = False
            , capacity = 2
            , path = ""
            }
          )
        , ( "342A"
          , { label = "342A Lounge"
            , colour = Blue
            , bookable = False
            , exception = False
            , capacity = 4
            , path = ""
            }
          )
        , ( "342B"
          , { label = "342B Office"
            , colour = Green
            , bookable = False
            , exception = False
            , capacity = 3
            , path = ""
            }
          )
        , ( "342C"
          , { label = "342C Telephone Room"
            , colour = Red
            , bookable = False
            , exception = False
            , capacity = 2
            , path = ""
            }
          )
        , ( "342D"
          , { label = "342D Small Talk Attic"
            , colour = Red
            , bookable = True
            , exception = False
            , capacity = 2
            , path = ""
            }
          )
        , ( "342E"
          , { label = "342E Office"
            , colour = Green
            , bookable = False
            , exception = False
            , capacity = 4
            , path = ""
            }
          )
        , ( "342F"
          , { label = "342F Group Room"
            , colour = Blue
            , bookable = False
            , exception = False
            , capacity = 8
            , path = ""
            }
          )
        , ( "343"
          , { label = "343 Miriams' Old Room"
            , colour = Yellow
            , bookable = True
            , exception = False
            , capacity = 1
            , path = ""
            }
          )
        , ( "344"
          , { label = "344 Bea's Old Room"
            , colour = Yellow
            , bookable = True
            , exception = False
            , capacity = 1
            , path = ""
            }
          )
        , ( "345"
          , { label = "345 Office"
            , colour = Yellow
            , bookable = False
            , exception = False
            , capacity = 1
            , path = ""
            }
          )
        , ( "346"
          , { label = "346 Office"
            , colour = Yellow
            , bookable = False
            , exception = False
            , capacity = 1
            , path = ""
            }
          )
        , ( "347"
          , { label = "347 Office"
            , colour = Yellow
            , bookable = False
            , exception = False
            , capacity = 1
            , path = ""
            }
          )
        , ( "348"
          , { label = "348 Office"
            , colour = Yellow
            , bookable = False
            , exception = False
            , capacity = 1
            , path = ""
            }
          )
        , ( "349"
          , { label = "349 Office"
            , colour = Yellow
            , bookable = False
            , exception = False
            , capacity = 3
            , path = ""
            }
          )
        , ( "350"
          , { label = "350 Office"
            , colour = Yellow
            , bookable = False
            , exception = False
            , capacity = 3
            , path = ""
            }
          )
        , ( "351"
          , { label = "351 PHD Crunch Time"
            , colour = Yellow
            , bookable = True
            , exception = False
            , capacity = 3
            , path = ""
            }
          )
        , ( "352"
          , { label = "352 Office"
            , colour = Yellow
            , bookable = False
            , exception = False
            , capacity = 2
            , path = ""
            }
          )
        , ( "355"
          , { label = "355 Corner South Attic"
            , colour = Red
            , bookable = True
            , exception = False
            , capacity = 8
            , path = ""
            }
          )
        , ( "402"
          , { label = "402 Shower"
            , colour = Clear
            , bookable = False
            , exception = False
            , capacity = 1
            , path = "M69.25 422h26.5v20l-26.5 25v-45z"
            }
          )
        , ( "406"
          , { label = "406 Office"
            , colour = Yellow
            , bookable = False
            , exception = False
            , capacity = 2
            , path = "M67 180.25h67.25v81h-41.2l-.4 50.2h-25.1L67 180.25z"
            }
          )
        , ( "407"
          , { label = "407 Office"
            , colour = Yellow
            , bookable = False
            , exception = False
            , capacity = 4
            , path = "M4.75 101.05h123l-.5 77.25-96.1-.65-.4-47.2h-26v-29.4z"
            }
          )
        , ( "408"
          , { label = "408 Office"
            , colour = Yellow
            , bookable = False
            , exception = False
            , capacity = 3
            , path = "M127.75 5.35h-36v23.3h-61v39.4h-26v31.5l123-2V5.35z"
            }
          )
        , ( "409"
          , { label = "409 Office"
            , colour = Yellow
            , bookable = False
            , exception = False
            , capacity = 4
            , path = "M223.05 5.35h-92.7l-.5 87.9h10.4v26.8h61.1l21.7-21.6V5.35z"
            }
          )
        , ( "410"
          , { label = "410 Telephone Room"
            , colour = Red
            , bookable = False
            , exception = False
            , capacity = 4
            , path = "M307.45 94.05h-23.4v-65.4h-55.9v69.3l20.8 21.2h46.4l12.1 3.9v-29z"
            }
          )
        , ( "412"
          , { label = "412 Office"
            , colour = Yellow
            , bookable = False
            , exception = False
            , capacity = 3
            , path = "M226.05 424.55h58v45.9h23.8v62.8h-23.8v36.8h-58v-63.3h1.3l.4-20.3h-1.7v-61.9z"
            }
          )
        , ( "413"
          , { label = "413 Office"
            , colour = Yellow
            , bookable = False
            , exception = False
            , capacity = 4
            , path = "M223.05 621.55l.8-114.3h-90.1v114.3h89.3z"
            }
          )
        , ( "414"
          , { label = "414 Office (Calle's Room)"
            , colour = Yellow
            , bookable = False
            , exception = True
            , capacity = 2
            , path = "M131.65 507.25h-47.2l-23.4 20.3v53.7h30.3v40.3h37.2l-.8-24.2 3.9-.5v-89.6z"
            }
          )
        , ( "415"
          , { label = "415 Toilet"
            , colour = Clear
            , bookable = False
            , exception = False
            , capacity = 1
            , path = "M30.25 422l36.75-.5.25 46.75-29.25 28h-7.75V422z"
            }
          )
        , ( "418"
          , { label = "418 Telephone Room"
            , colour = Red
            , bookable = False
            , exception = True
            , capacity = 2
            , path = "M67.55 365.85h66.2v43.7h-66.2v-43.7z"
            }
          )
        ]
