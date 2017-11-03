module Main exposing (..)

import Html exposing (Html, text, hr, h1, h2, h3, h4, h5)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onDoubleClick)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Card as Card
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Form.Input as Input
import DnDList


main =
    Html.beginnerProgram
        { model = init3
        , update = update
        , view = view
        }


init =
    Model (DnDList.init [] "") (DnDList.init [] 0) Nothing


init3 =
    Model
        (DnDList.init [ "zero", "two", "three" ] "")
        (DnDList.init [ 1, 2, 3 ] 0)
        Nothing


type alias Model =
    { strDnDList : DnDList.Model String
    , intDnDList : DnDList.Model Int
    , blockDeselect : Maybe ListType
    }


type Msg
    = Deselect
    | BlockDeselect ListType
    | Click ListType Int
    | Edit ListType Int


type ListType
    = StrList
    | IntList


update msg model =
    case msg of
        Deselect ->
            case model.blockDeselect of
                Just listType ->
                    case listType of
                        StrList ->
                            { model
                                | intDnDList =
                                    model.intDnDList
                                        |> DnDList.update DnDList.Deselect
                                , blockDeselect = Nothing
                            }

                        IntList ->
                            { model
                                | strDnDList =
                                    model.strDnDList
                                        |> DnDList.update DnDList.Deselect
                                , blockDeselect = Nothing
                            }

                Nothing ->
                    { model
                        | strDnDList =
                            model.strDnDList
                                |> DnDList.update DnDList.Deselect
                        , intDnDList =
                            model.intDnDList
                                |> DnDList.update DnDList.Deselect
                    }

        BlockDeselect listType ->
            { model | blockDeselect = Just listType }

        Click listType i ->
            case listType of
                StrList ->
                    { model
                        | blockDeselect = Just StrList
                        , strDnDList =
                            model.strDnDList
                                |> DnDList.update (DnDList.SingleSelect i)
                    }

                IntList ->
                    { model
                        | blockDeselect = Just IntList
                        , intDnDList =
                            model.intDnDList
                                |> DnDList.update (DnDList.SingleSelect i)
                    }

        Edit listType i ->
            case listType of
                StrList ->
                    { model
                        | blockDeselect = Just StrList
                        , strDnDList =
                            model.strDnDList
                                |> DnDList.update (DnDList.Editing i)
                    }

                IntList ->
                    { model
                        | blockDeselect = Just IntList
                        , intDnDList =
                            model.intDnDList
                                |> DnDList.update (DnDList.Editing i)
                    }


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


strItemEditView : Int -> String -> ListGroup.Item Msg
strItemEditView i value =
    ListGroup.li
        [ ListGroup.attrs [ (BlockDeselect StrList) |> onClick ] ]
        [ Input.text [ Input.value value ] ]


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


intItemEditView : Int -> Int -> ListGroup.Item Msg
intItemEditView i value =
    ListGroup.li
        [ ListGroup.attrs [ (BlockDeselect IntList) |> onClick ] ]
        [ Input.number [ value |> toString |> Input.value ] ]
