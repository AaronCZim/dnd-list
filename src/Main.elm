module Main exposing (..)

import Html exposing (Html, text, hr, h1, h2, h3, h4, h5)
import Html.Attributes exposing (style, class)
import Html.Events exposing (..)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Card as Card
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Form.Input as Input
import DnDList
import Keyboard exposing (KeyCode)
import Char
import Task
import SelectDom
import Mouse


main =
    Html.program
        { init = init3
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init3 =
    ( init3Model, Cmd.none )


init3Model =
    Model
        (DnDList.init [ "one", "two", "three" ] "")
        (DnDList.init [ 1, 2, 3 ] 0)
        Nothing
        OutOfBounds


selectStrEditor =
    SelectDom.select ".strEditor" |> Task.attempt (\_ -> NoOp)


selectIntEditor =
    SelectDom.select ".intEditor" |> Task.attempt (\_ -> NoOp)


subscriptions model =
    Keyboard.presses <| \key -> Keydown key


type alias Model =
    { strDnDList : DnDList.Model String String
    , intDnDList : DnDList.Model Int String
    , blockDeselect : Maybe ListType
    , mouse : MouseState
    }


type MouseState
    = OutOfBounds
    | Hover ListType Int
    | ClickHold ListType Int
    | Drag ListType Int
    | DragOutOfBounds ListType


type Msg
    = NoOp
    | Deselect
    | BlockDeselect ListType
    | Keydown KeyCode
    | Click ListType Int
    | Edit ListType Int
    | StrEdit String
    | IntEdit String
    | Enter ListType Int
    | Leave
    | MouseMove Mouse.Event
    | MouseDown Mouse.Event
    | MouseUp Mouse.Event


type ListType
    = StrList
    | IntList


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Deselect ->
            case model.blockDeselect of
                Just listType ->
                    case listType of
                        StrList ->
                            ( { model
                                | intDnDList =
                                    model.intDnDList
                                        |> DnDList.update DnDList.Deselect
                                , blockDeselect = Nothing
                              }
                            , Cmd.none
                            )

                        IntList ->
                            ( { model
                                | strDnDList =
                                    model.strDnDList
                                        |> DnDList.update DnDList.Deselect
                                , blockDeselect = Nothing
                              }
                            , Cmd.none
                            )

                Nothing ->
                    ( { model
                        | strDnDList =
                            model.strDnDList
                                |> DnDList.update DnDList.Deselect
                        , intDnDList =
                            model.intDnDList
                                |> DnDList.update DnDList.Deselect
                      }
                    , Cmd.none
                    )

        BlockDeselect listType ->
            ( { model | blockDeselect = Just listType }
            , Cmd.none
            )

        Keydown keyCode ->
            if keyCode == 13 then
                ( { model
                    | strDnDList =
                        model.strDnDList
                            |> DnDList.update (DnDList.CommitEdit (\str -> Ok str))
                    , intDnDList =
                        model.intDnDList
                            |> DnDList.update (DnDList.CommitEdit String.toInt)
                  }
                , Cmd.none
                )
            else
                ( model, Cmd.none )

        Click listType i ->
            case listType of
                StrList ->
                    ( { model
                        | blockDeselect = Just StrList
                        , strDnDList =
                            model.strDnDList
                                |> DnDList.update (DnDList.SingleSelect i)
                      }
                    , Cmd.none
                    )

                IntList ->
                    ( { model
                        | blockDeselect = Just IntList
                        , intDnDList =
                            model.intDnDList
                                |> DnDList.update (DnDList.SingleSelect i)
                      }
                    , Cmd.none
                    )

        Edit listType i ->
            case listType of
                StrList ->
                    ( { model
                        | blockDeselect = Just StrList
                        , strDnDList =
                            model.strDnDList
                                |> DnDList.update
                                    (DnDList.Editing identity i)
                      }
                    , selectStrEditor
                    )

                IntList ->
                    ( { model
                        | blockDeselect = Just IntList
                        , intDnDList =
                            model.intDnDList
                                |> DnDList.update
                                    (DnDList.Editing toString i)
                      }
                    , selectIntEditor
                    )

        StrEdit str ->
            ( { model
                | strDnDList =
                    model.strDnDList
                        |> DnDList.update (DnDList.Edition str)
              }
            , Cmd.none
            )

        IntEdit str ->
            ( { model
                | intDnDList =
                    model.intDnDList
                        |> DnDList.update (DnDList.Edition str)
              }
            , Cmd.none
            )

        Enter listType i ->
            case model.mouse of
                Drag _ _ ->
                    applyDrag listType i model

                DragOutOfBounds originListType ->
                    if originListType == listType then
                        applyDrag listType i model
                    else
                        ( model, Cmd.none )

                _ ->
                    ( { model | mouse = Hover listType i }, Cmd.none )

        Leave ->
            case model.mouse of
                Drag listType i ->
                    applyDragOutOfBounds listType model

                _ ->
                    ( { model | mouse = OutOfBounds }, Cmd.none )

        MouseMove event ->
            case model.mouse of
                ClickHold listType i ->
                    applyDrag listType i model

                _ ->
                    ( model, Cmd.none )

        MouseDown event ->
            case model.mouse of
                Hover listType i ->
                    ( { model | mouse = ClickHold listType i }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        MouseUp event ->
            case model.mouse of
                Drag listType i ->
                    case listType of
                        StrList ->
                            ( { model
                                | mouse = Hover listType i
                                , strDnDList =
                                    model.strDnDList
                                        |> DnDList.update DnDList.Drop
                              }
                            , Cmd.none
                            )

                        IntList ->
                            ( { model
                                | mouse = Hover listType i
                                , intDnDList =
                                    model.intDnDList
                                        |> DnDList.update DnDList.Drop
                              }
                            , Cmd.none
                            )

                DragOutOfBounds listType ->
                    case listType of
                        StrList ->
                            ( { model
                                | mouse = OutOfBounds
                                , strDnDList =
                                    model.strDnDList
                                        |> DnDList.update DnDList.Drop
                              }
                            , Cmd.none
                            )

                        IntList ->
                            ( { model
                                | mouse = OutOfBounds
                                , intDnDList =
                                    model.intDnDList
                                        |> DnDList.update DnDList.Drop
                              }
                            , Cmd.none
                            )

                ClickHold listType i ->
                    ( { model | mouse = Hover listType i }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


applyDrag listType i model =
    case listType of
        StrList ->
            ( { model
                | mouse = Drag listType i
                , strDnDList =
                    model.strDnDList
                        |> DnDList.update (DnDList.Drag i)
              }
            , Cmd.none
            )

        IntList ->
            ( { model
                | mouse = Drag listType i
                , intDnDList =
                    model.intDnDList
                        |> DnDList.update (DnDList.Drag i)
              }
            , Cmd.none
            )


applyDragOutOfBounds listType model =
    case listType of
        StrList ->
            ( { model
                | mouse = DragOutOfBounds listType
                , strDnDList =
                    model.strDnDList
                        |> DnDList.update DnDList.DragOutOfBounds
              }
            , Cmd.none
            )

        IntList ->
            ( { model
                | mouse = DragOutOfBounds listType
                , intDnDList =
                    model.intDnDList
                        |> DnDList.update DnDList.DragOutOfBounds
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    Grid.container
        [ style [ ( "margin-top", "20px" ) ]
        , Deselect |> onClick
        ]
        [ CDN.stylesheet
        , Grid.row []
            [ Grid.col [ Col.xs6 ]
                [ Card.config []
                    |> Card.header []
                        [ h5 [] [ text "Text Drag and Drop List" ] ]
                    |> (DnDList.view
                            { default = strView
                            , selected = strActiveView
                            , editing = strEditView
                            , caret = caret StrList
                            }
                            model.strDnDList
                            |> Card.listGroup
                       )
                    |> Card.view
                ]
            , Grid.col [ Col.xs6 ]
                [ Card.config []
                    |> Card.header []
                        [ h5 [] [ text "Number Drag and Drop List" ] ]
                    |> (DnDList.view
                            { default = intView
                            , selected = intActiveView
                            , editing = intEditView
                            , caret = caret IntList
                            }
                            model.intDnDList
                            |> Card.listGroup
                       )
                    |> Card.view
                ]
            ]
        , Grid.row []
            [ Grid.col [ Col.xs12 ] [ model |> toString |> text ] ]
        ]


type alias Card =
    Card.Config Msg -> Card.Config Msg


strView : Int -> String -> ListGroup.Item Msg
strView i value =
    ListGroup.li
        [ ListGroup.attrs
            [ Click StrList i |> onClick
            , Edit StrList i |> onDoubleClick
            , Enter StrList i |> onMouseEnter
            , Leave |> onMouseLeave
            , MouseMove |> Mouse.onMove
            , MouseDown |> Mouse.onDown
            , MouseUp |> Mouse.onUp
            ]
        ]
        [ value |> text ]


strActiveView : Int -> String -> ListGroup.Item Msg
strActiveView i value =
    ListGroup.li
        [ ListGroup.attrs
            [ Click StrList i |> onClick
            , Edit StrList i |> onDoubleClick
            , Enter StrList i |> onMouseEnter
            , Leave |> onMouseLeave
            , MouseMove |> Mouse.onMove
            , MouseDown |> Mouse.onDown
            , MouseUp |> Mouse.onUp
            ]
        , ListGroup.active
        ]
        [ value |> text ]


strEditView : String -> Int -> String -> ListGroup.Item Msg
strEditView newValue i value =
    ListGroup.li
        [ ListGroup.attrs
            [ (BlockDeselect StrList) |> onClick
            , StrEdit |> onInput
            , Enter StrList i |> onMouseEnter
            , Leave |> onMouseLeave
            ]
        ]
        [ Input.text
            [ Input.attrs [ class "strEditor" ]
            , Input.value newValue
            ]
        ]


intView : Int -> Int -> ListGroup.Item Msg
intView i value =
    ListGroup.li
        [ ListGroup.attrs
            [ Click IntList i |> onClick
            , Edit IntList i |> onDoubleClick
            , Enter IntList i |> onMouseEnter
            , Leave |> onMouseLeave
            , MouseMove |> Mouse.onMove
            , MouseDown |> Mouse.onDown
            , MouseUp |> Mouse.onUp
            ]
        ]
        [ value |> toString |> text ]


intActiveView : Int -> Int -> ListGroup.Item Msg
intActiveView i value =
    ListGroup.li
        [ ListGroup.attrs
            [ Click IntList i |> onClick
            , Edit IntList i |> onDoubleClick
            , Enter IntList i |> onMouseEnter
            , Leave |> onMouseLeave
            , MouseMove |> Mouse.onMove
            , MouseDown |> Mouse.onDown
            , MouseUp |> Mouse.onUp
            ]
        , ListGroup.active
        ]
        [ value |> toString |> text ]


intEditView : String -> Int -> Int -> ListGroup.Item Msg
intEditView newValue i value =
    ListGroup.li
        [ ListGroup.attrs
            [ BlockDeselect IntList |> onClick
            , IntEdit |> onInput
            , Enter IntList i |> onMouseEnter
            , Leave |> onMouseLeave
            ]
        ]
        [ Input.number
            [ Input.attrs [ class "intEditor" ]
            , newValue |> Input.value
            ]
        ]


caret : ListType -> Int -> ListGroup.Item Msg
caret listType i =
    ListGroup.li
        [ ListGroup.attrs
            [ style
                [ ( "margin", "0" )
                , ( "padding", "0" )
                , ( "height", "8px" )
                ]
            , Enter listType i |> onMouseEnter
            , Leave |> onMouseLeave
            , MouseMove |> Mouse.onMove
            , MouseDown |> Mouse.onDown
            , MouseUp |> Mouse.onUp
            ]
        , ListGroup.active
        ]
        []
