module DnDList exposing (..)


init : List a -> a -> Model a
init list defaultValue =
    Model
        (SelectionModel
            { list = list, selection = EmptySelection }
        )
        defaultValue


type alias Model a =
    { body : ModelBody a
    , defaultValue : a
    }


type ModelBody a
    = SelectionModel
        { list : List a
        , selection : Selection
        }
    | EditModel
        { first : List a
        , editing : a
        , last : List a
        }


type Selection
    = EmptySelection
    | SingleSelection Int


type Msg a
    = Deselect
    | SingleSelect Int
    | Editing Int
    | Edition a
    | CommitEdit


update : Msg a -> Model a -> Model a
update msg model =
    case model.body of
        SelectionModel selectionModel ->
            case msg of
                Deselect ->
                    applyDeselect model selectionModel.list

                SingleSelect i ->
                    applySingleSelect i model selectionModel.list

                Editing i ->
                    applyEditing i model selectionModel.list

                Edition value ->
                    applyEdition value model selectionModel.list

                _ ->
                    model

        EditModel editModel ->
            case msg of
                Deselect ->
                    editModel
                        |> listFromEditModel
                        |> applyDeselect model

                SingleSelect i ->
                    editModel
                        |> listFromEditModel
                        |> applySingleSelect i model

                Editing i ->
                    if List.length editModel.first == i then
                        model
                    else
                        editModel
                            |> listFromEditModel
                            |> applyEditing i model

                Edition value ->
                    editModel
                        |> listFromEditModel
                        |> applyEdition value model

                CommitEdit ->
                    { model
                        | body =
                            SelectionModel
                                { list = listFromEditModel editModel
                                , selection = EmptySelection
                                }
                    }


applyDeselect : Model a -> List a -> Model a
applyDeselect model list =
    { model
        | body =
            SelectionModel
                { list = list
                , selection = EmptySelection
                }
    }


applySingleSelect : Int -> Model a -> List a -> Model a
applySingleSelect i model list =
    if i >= 0 && i < List.length list then
        { model
            | body =
                SelectionModel
                    { list = list
                    , selection = SingleSelection i
                    }
        }
    else
        model


applyEditing : Int -> Model a -> List a -> Model a
applyEditing i model list =
    { model
        | body =
            EditModel
                { first = list |> List.take i
                , editing =
                    list
                        |> List.drop i
                        |> List.head
                        |> Maybe.withDefault
                            model.defaultValue
                , last = list |> List.drop (i + 1)
                }
    }


applyEdition : a -> Model a -> List a -> Model a
applyEdition value model list =
    case model.body of
        EditModel editModel ->
            { model
                | body =
                    EditModel
                        { editModel | editing = value }
            }

        _ ->
            model


list : Model a -> List a
list model =
    case model.body of
        SelectionModel { list, selection } ->
            list

        EditModel editModel ->
            listFromEditModel editModel


listFromEditModel { first, editing, last } =
    first ++ (editing :: last)


selection : Model a -> Selection
selection model =
    case model.body of
        SelectionModel { selection } ->
            selection

        EditModel _ ->
            EmptySelection


inSelection : Selection -> Int -> Bool
inSelection selection i =
    case selection of
        EmptySelection ->
            False

        SingleSelection j ->
            i == j


type alias ItemViewType a b =
    { default : Int -> a -> b
    , selected : Int -> a -> b
    , editing : Int -> a -> b
    }


view : ItemViewType a b -> Model a -> List b
view itemViewType model =
    case model.body of
        SelectionModel { list, selection } ->
            list
                |> List.indexedMap
                    (itemSelectionView itemViewType selection)

        EditModel editModel ->
            let
                first =
                    editModel.first
                        |> List.indexedMap itemViewType.default

                editing =
                    editModel.editing
                        |> itemViewType.editing
                            (List.length editModel.first)

                last =
                    editModel.last
                        |> List.indexedMap
                            ((addMap (List.length editModel.first + 1))
                                itemViewType.default
                            )
            in
                (first ++ (editing :: last))


addMap : Int -> (Int -> a -> b) -> Int -> a -> b
addMap offset itemView i value =
    itemView (i + offset) value


itemSelectionView itemViewType selection i value =
    if i |> inSelection selection then
        itemViewType.selected i value
    else
        itemViewType.default i value
