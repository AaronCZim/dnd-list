module DnDList exposing (..)


init : List a -> a -> Model a b
init list defaultValue =
    Model
        (SelectionModel
            { list = list, selection = EmptySelection }
        )
        defaultValue


type alias Model a b =
    { body : ModelBody a b
    , defaultValue : a
    }


type ModelBody a b
    = SelectionModel
        { list : List a
        , selection : Selection
        }
    | EditModel
        { first : List a
        , editing : a
        , edition : b
        , last : List a
        }


type Selection
    = EmptySelection
    | SingleSelection Int


type Msg a b
    = Deselect
    | SingleSelect Int
    | Editing (a -> b) Int
    | Edition b
    | CommitEdit (b -> Result String a)


update : Msg a b -> Model a b -> Model a b
update msg model =
    case model.body of
        SelectionModel selectionModel ->
            case msg of
                Deselect ->
                    applyDeselect model selectionModel.list

                SingleSelect i ->
                    applySingleSelect i model selectionModel.list

                Editing editionStart i ->
                    applyEditing editionStart
                        i
                        model
                        selectionModel.list

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

                Editing editionStart i ->
                    if List.length editModel.first == i then
                        model
                    else
                        editModel
                            |> listFromEditModel
                            |> applyEditing editionStart i model

                Edition value ->
                    editModel
                        |> listFromEditModel
                        |> applyEdition value model

                CommitEdit toValue ->
                    let
                        newValue =
                            toValue editModel.edition
                    in
                        case newValue of
                            Err err ->
                                { model
                                    | body =
                                        SelectionModel
                                            { list = listFromEditModel editModel
                                            , selection = EmptySelection
                                            }
                                }

                            Ok value ->
                                { model
                                    | body =
                                        SelectionModel
                                            { list =
                                                editModel.first
                                                    ++ (value :: editModel.last)
                                            , selection = EmptySelection
                                            }
                                }


applyDeselect : Model a b -> List a -> Model a b
applyDeselect model list =
    { model
        | body =
            SelectionModel
                { list = list
                , selection = EmptySelection
                }
    }


applySingleSelect : Int -> Model a b -> List a -> Model a b
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


applyEditing : (a -> b) -> Int -> Model a b -> List a -> Model a b
applyEditing editionStart i model list =
    let
        editing =
            list
                |> List.drop i
                |> List.head
                |> Maybe.withDefault
                    model.defaultValue
    in
        { model
            | body =
                EditModel
                    { first = list |> List.take i
                    , editing = editing
                    , edition = editionStart editing
                    , last = list |> List.drop (i + 1)
                    }
        }


applyEdition : b -> Model a b -> List a -> Model a b
applyEdition value model list =
    case model.body of
        EditModel editModel ->
            { model
                | body =
                    EditModel
                        { editModel | edition = value }
            }

        _ ->
            model


list : Model a b -> List a
list model =
    case model.body of
        SelectionModel { list, selection } ->
            list

        EditModel editModel ->
            listFromEditModel editModel


listFromEditModel { first, editing, last } =
    first ++ (editing :: last)


selection : Model a b -> Selection
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


type alias ItemViewType a b c =
    { default : Int -> a -> c
    , selected : Int -> a -> c
    , editing : b -> Int -> a -> c
    }


view : ItemViewType a b c -> Model a b -> List c
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
                            editModel.edition
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
