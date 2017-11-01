module Main exposing (..)

import Html exposing (Html, text, hr, h1, h2, h3, h4, h5)
import Html.Attributes
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Card as Card
import Bootstrap.ListGroup as ListGroup


main =
    Html.beginnerProgram
        { model = init3
        , update = update
        , view = view
        }


init =
    Model [] []


init3 =
    Model [ "one", "two", "three" ] [ 1, 2, 3 ]


type alias Model =
    { strDnDList : List String
    , intDnDList : List Int
    }


update msg model =
    model


view : Model -> Html msg
view model =
    Grid.container [ Html.Attributes.style [ ( "margin-top", "20px" ) ] ]
        [ CDN.stylesheet
        , Grid.row []
            [ Grid.col [ Col.xs6 ]
                [ Card.config []
                    |> Card.header [] [ h5 [] [ text "Text Drag and Drop List" ] ]
                    |> listView strView model.strDnDList
                    |> Card.view
                ]
            , Grid.col [ Col.xs6 ]
                [ Card.config []
                    |> Card.header []
                        [ h5 [] [ text "Number Drag and Drop List" ]
                        ]
                    |> listView numView model.intDnDList
                    |> Card.view
                ]
            ]
        ]


listView : (a -> ListGroup.Item msg) -> List a -> Card.Config msg -> Card.Config msg
listView itemView list =
    list
        |> List.map itemView
        |> Card.listGroup


numView : Int -> ListGroup.Item msg
numView value =
    ListGroup.li [] [ value |> toString |> text ]


strView : String -> ListGroup.Item msg
strView value =
    ListGroup.li [] [ value |> text ]
