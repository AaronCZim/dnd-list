module Main exposing (..)

import Html exposing (Html, text, hr, h1, h2, h3, h4, h5)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Card as Card
import Bootstrap.ListGroup as ListGroup
import DnDList


main =
    Html.beginnerProgram
        { model = init3
        , update = update
        , view = view
        }


init =
    Model (DnDList.init []) (DnDList.init [])


init3 =
    Model (DnDList.init [ "one", "two", "three" ]) (DnDList.init [ 1, 2, 3 ])


type alias Model =
    { strDnDList : DnDList.Model String
    , intDnDList : DnDList.Model Int
    }


type Msg
    = Click ListType Int


type ListType
    = StrList
    | IntList


update msg model =
    case msg of
        Click listType i ->
            case listType of
                StrList ->
                    { model
                        | strDnDList =
                            model.strDnDList
                                |> DnDList.update (DnDList.SingleSelect i)
                    }

                IntList ->
                    { model
                        | intDnDList =
                            model.intDnDList
                                |> DnDList.update (DnDList.SingleSelect i)
                    }


view : Model -> Html Msg
view model =
    Grid.container
        [ style [ ( "margin-top", "20px" ) ] ]
        [ CDN.stylesheet
        , Grid.row []
            [ Grid.col [ Col.xs6 ]
                [ Card.config []
                    |> Card.header []
                        [ h5 [] [ text "Text Drag and Drop List" ] ]
                    |> (model.strDnDList
                            |> DnDList.list
                            |> listView
                                (DnDList.selection model.strDnDList)
                                strView
                       )
                    |> Card.view
                ]
            , Grid.col [ Col.xs6 ]
                [ Card.config []
                    |> Card.header []
                        [ h5 [] [ text "Number Drag and Drop List" ] ]
                    |> (model.intDnDList
                            |> DnDList.list
                            |> listView
                                (DnDList.selection model.intDnDList)
                                intView
                       )
                    |> Card.view
                ]
            ]
        , Grid.row []
            [ Grid.col [ Col.xs12 ] [ model |> toString |> text ] ]
        ]


type alias CardItem =
    Card.Config Msg -> Card.Config Msg


type alias ListItemView a =
    DnDList.Selection -> Int -> a -> ListGroup.Item Msg


listView : DnDList.Selection -> ListItemView a -> List a -> CardItem
listView selection itemView list =
    list
        |> List.indexedMap (itemView selection)
        |> Card.listGroup


strView : DnDList.Selection -> Int -> String -> ListGroup.Item Msg
strView selection i value =
    ListGroup.li
        ([ ListGroup.attrs [ (Click StrList i) |> onClick ] ]
            ++ if DnDList.inSelection i selection then
                [ ListGroup.active ]
               else
                []
        )
        [ value |> text ]


intView : DnDList.Selection -> Int -> Int -> ListGroup.Item Msg
intView selection i value =
    ListGroup.li
        ([ ListGroup.attrs [ (Click IntList i) |> onClick ] ]
            ++ if DnDList.inSelection i selection then
                [ ListGroup.active ]
               else
                []
        )
        [ value |> toString |> text ]
