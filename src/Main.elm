module Main exposing (..)

import Html exposing (Html, text, hr, h1, h2, h3, h4, h5)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onDoubleClick, onInput)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Card as Card
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Form.Input as Input
import DnDList
import Keyboard exposing (KeyCode)
import Char


main =
    Html.program
        { init = init3
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init =
    ( initModel, Cmd.none )


initModel =
    Model (DnDList.init [] "") (DnDList.init [] 0) Nothing


init3 =
    ( init3Model, Cmd.none )


init3Model =
    Model
        (DnDList.init [ "one", "two", "three" ] "")
        (DnDList.init [ 1, 2, 3 ] 0)
        Nothing


subscriptions model =
    Keyboard.presses <| \key -> Keydown key


type alias Model =
    { strDnDList : DnDList.Model String String
    , intDnDList : DnDList.Model Int String
    , blockDeselect : Maybe ListType
    }


type Msg
    = Deselect
    | BlockDeselect ListType
    | Keydown KeyCode
    | Click ListType Int
    | Edit ListType Int
    | StrEdit String
    | IntEdit String


type ListType
    = StrList
    | IntList


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
                    , Cmd.none
                    )

                IntList ->
                    ( { model
                        | blockDeselect = Just IntList
                        , intDnDList =
                            model.intDnDList
                                |> DnDList.update
                                    (DnDList.Editing toString i)
                      }
                    , Cmd.none
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
                            { default = strItemView
                            , selected = strItemActiveView
                            , editing = strItemEditView
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
                            { default = intItemView
                            , selected = intItemActiveView
                            , editing = intItemEditView
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


type alias CardItem =
    Card.Config Msg -> Card.Config Msg


strItemView : Int -> String -> ListGroup.Item Msg
strItemView i value =
    ListGroup.li
        [ ListGroup.attrs
            [ (Click StrList i) |> onClick
            , (Edit StrList i) |> onDoubleClick
            ]
        ]
        [ value |> text ]


strItemActiveView : Int -> String -> ListGroup.Item Msg
strItemActiveView i value =
    ListGroup.li
        ([ ListGroup.attrs
            [ (Click StrList i) |> onClick
            , (Edit StrList i) |> onDoubleClick
            ]
         , ListGroup.active
         ]
        )
        [ value |> text ]


strItemEditView : String -> Int -> String -> ListGroup.Item Msg
strItemEditView newValue i value =
    ListGroup.li
        [ ListGroup.attrs
            [ (BlockDeselect StrList) |> onClick
            , StrEdit |> onInput
            ]
        ]
        [ Input.text [ Input.value newValue ] ]


intItemView : Int -> Int -> ListGroup.Item Msg
intItemView i value =
    ListGroup.li
        [ ListGroup.attrs
            [ (Click IntList i) |> onClick
            , (Edit IntList i) |> onDoubleClick
            ]
        ]
        [ value |> toString |> text ]


intItemActiveView : Int -> Int -> ListGroup.Item Msg
intItemActiveView i value =
    ListGroup.li
        [ ListGroup.attrs
            [ (Click IntList i) |> onClick
            , (Edit IntList i) |> onDoubleClick
            ]
        , ListGroup.active
        ]
        [ value |> toString |> text ]


intItemEditView : String -> Int -> Int -> ListGroup.Item Msg
intItemEditView newValue i value =
    ListGroup.li
        [ ListGroup.attrs
            [ (BlockDeselect IntList) |> onClick
            , IntEdit |> onInput
            ]
        ]
        [ Input.number [ newValue |> Input.value ] ]
