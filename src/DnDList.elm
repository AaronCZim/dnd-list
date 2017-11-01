module DnDList exposing (..)


init list =
    Model list EmptySelection


type alias Model a =
    { list : List a
    , selection : Selection
    }


type Selection
    = EmptySelection
    | SingleSelection Int


type Msg
    = Deselect
    | SingleSelect Int


update msg model =
    case msg of
        Deselect ->
            { model | selection = EmptySelection }

        SingleSelect i ->
            { model | selection = SingleSelection i }


list : Model a -> List a
list model =
    model.list


selection : Model a -> Selection
selection model =
    model.selection


inSelection : Int -> Selection -> Bool
inSelection i selection =
    case selection of
        EmptySelection ->
            False

        SingleSelection j ->
            i == j
