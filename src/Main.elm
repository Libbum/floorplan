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
          , { label = "106 Meeting Room"
            , colour = Red
            , bookable = True
            , exception = False
            , capacity = 0
            , path = "M227 222h102.6v66.6h-102.6v-66.6z"
            }
          )
        , ( "108"
          , { label = "108 Archive"
            , colour = Clear
            , bookable = False
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "109"
          , { label = "109 Secure Archive"
            , colour = Clear
            , bookable = False
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "110"
          , { label = "110 Meditation Room"
            , colour = Clear
            , bookable = False
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "206"
          , { label = "206 Telephone Room"
            , colour = Yellow
            , bookable = False
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "208B"
          , { label = "208B Telephone Room"
            , colour = Yellow
            , bookable = False
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "208"
          , { label = "208 Hall"
            , colour = Blue
            , bookable = False
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "209"
          , { label = "209 Group Room"
            , colour = Blue
            , bookable = False
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "211"
          , { label = "211 Office"
            , colour = Green
            , bookable = False
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "212"
          , { label = "212 Office"
            , colour = Green
            , bookable = False
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "213"
          , { label = "213 Office (Admin Room)"
            , colour = Green
            , bookable = False
            , exception = True
            , capacity = 0
            , path = ""
            }
          )
        , ( "214"
          , { label = "214 Office"
            , colour = Green
            , bookable = False
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "216"
          , { label = "216 Lobby"
            , colour = Blue
            , bookable = False
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "222"
          , { label = "222 Massage/Sick Room"
            , colour = Yellow
            , bookable = False
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "226"
          , { label = "226 Meeting Room"
            , colour = Red
            , bookable = True
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "237"
          , { label = "237 Meeting Room"
            , colour = Red
            , bookable = True
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "238A"
          , { label = "238A Meeting Room"
            , colour = Red
            , bookable = True
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "238B"
          , { label = "238B Meeting Room"
            , colour = Red
            , bookable = True
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "238C"
          , { label = "238C Group Room"
            , colour = Blue
            , bookable = False
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "239"
          , { label = "239 Meeting Room"
            , colour = Red
            , bookable = True
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "240"
          , { label = "240 Kitchen"
            , colour = Yellow
            , bookable = False
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "248"
          , { label = "248 Meeting Room"
            , colour = Red
            , bookable = True
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "249"
          , { label = "249 Group Room"
            , colour = Blue
            , bookable = False
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "250"
          , { label = "250 Meeting Room"
            , colour = Red
            , bookable = True
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "251"
          , { label = "251 Lecture Room"
            , colour = Red
            , bookable = True
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "306"
          , { label = "306 Group Room"
            , colour = Blue
            , bookable = False
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "307"
          , { label = "307 Thorsten's Room"
            , colour = Red
            , bookable = False
            , exception = True
            , capacity = 0
            , path = ""
            }
          )
        , ( "308"
          , { label = "308 Group Room"
            , colour = Blue
            , bookable = False
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "310"
          , { label = "310 Telephone Room"
            , colour = Red
            , bookable = True
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "312"
          , { label = "312 Library"
            , colour = Yellow
            , bookable = False
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "313"
          , { label = "313 Meeting Room"
            , colour = Red
            , bookable = True
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "314"
          , { label = "314 Office"
            , colour = Green
            , bookable = False
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "315"
          , { label = "315 Office"
            , colour = Green
            , bookable = False
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "316"
          , { label = "316 Telephone Room"
            , colour = Red
            , bookable = True
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "321"
          , { label = "321 Meeting Room"
            , colour = Red
            , bookable = True
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "324"
          , { label = "324 Lounge"
            , colour = Blue
            , bookable = False
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "324B"
          , { label = "324B Group Room"
            , colour = Green
            , bookable = False
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "324C"
          , { label = "324C Office"
            , colour = Green
            , bookable = False
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "324D"
          , { label = "324D Office (SRC Leadership Representation Room)"
            , colour = Green
            , bookable = False
            , exception = True
            , capacity = 0
            , path = ""
            }
          )
        , ( "325"
          , { label = "325 Office (Malin's Room)"
            , colour = Yellow
            , bookable = False
            , exception = True
            , capacity = 0
            , path = ""
            }
          )
        , ( "326"
          , { label = "326 Office"
            , colour = Yellow
            , bookable = False
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "327"
          , { label = "327 Office"
            , colour = Yellow
            , bookable = False
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "328"
          , { label = "328 Office"
            , colour = Yellow
            , bookable = False
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "329"
          , { label = "329 Office"
            , colour = Yellow
            , bookable = False
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "330"
          , { label = "330 Office"
            , colour = Yellow
            , bookable = False
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "331"
          , { label = "331 Office"
            , colour = Yellow
            , bookable = False
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "332"
          , { label = "332 Office"
            , colour = Yellow
            , bookable = False
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "334"
          , { label = "334 Office"
            , colour = Yellow
            , bookable = False
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "342A"
          , { label = "342A Lounge"
            , colour = Blue
            , bookable = False
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "342B"
          , { label = "342B Office"
            , colour = Green
            , bookable = False
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "342C"
          , { label = "342C Telephone Room"
            , colour = Red
            , bookable = True
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "342D"
          , { label = "342D Telephone Room"
            , colour = Red
            , bookable = True
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "342E"
          , { label = "342E Office"
            , colour = Green
            , bookable = False
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "342F"
          , { label = "342F Group Room"
            , colour = Blue
            , bookable = False
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "343"
          , { label = "343 Office"
            , colour = Yellow
            , bookable = False
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "344"
          , { label = "344 Office"
            , colour = Yellow
            , bookable = False
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "345"
          , { label = "345 Office"
            , colour = Yellow
            , bookable = False
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "346"
          , { label = "346 Office"
            , colour = Yellow
            , bookable = False
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "347"
          , { label = "347 Office"
            , colour = Yellow
            , bookable = False
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "348"
          , { label = "348 Office"
            , colour = Yellow
            , bookable = False
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "349"
          , { label = "349 Office"
            , colour = Yellow
            , bookable = False
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "350"
          , { label = "350 Office"
            , colour = Yellow
            , bookable = False
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "351"
          , { label = "351 Office"
            , colour = Yellow
            , bookable = False
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "352"
          , { label = "352 Office"
            , colour = Yellow
            , bookable = False
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "355"
          , { label = "355 Meeting Room"
            , colour = Red
            , bookable = True
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "406"
          , { label = "406 Office"
            , colour = Yellow
            , bookable = False
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "407"
          , { label = "407 Office"
            , colour = Yellow
            , bookable = False
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "408"
          , { label = "408 Office"
            , colour = Yellow
            , bookable = False
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "409"
          , { label = "409 Office"
            , colour = Yellow
            , bookable = False
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "410"
          , { label = "410 Telephone Room"
            , colour = Red
            , bookable = True
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "412"
          , { label = "412 Office"
            , colour = Yellow
            , bookable = False
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "413"
          , { label = "413 Office"
            , colour = Yellow
            , bookable = False
            , exception = False
            , capacity = 0
            , path = ""
            }
          )
        , ( "414"
          , { label = "414 Office (Calle's Room)"
            , colour = Yellow
            , bookable = False
            , exception = True
            , capacity = 0
            , path = ""
            }
          )
        , ( "418"
          , { label = "418 Telephone Room"
            , colour = Red
            , bookable = True
            , exception = True
            , capacity = 0
            , path = ""
            }
          )
        ]
