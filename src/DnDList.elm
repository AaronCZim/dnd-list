module DnDList exposing (..)


init : List itemType -> itemType -> Model itemType itemStruct
init list defaultValue =
    Model
        (SelectionModel
            { list = list, selection = EmptySelection }
        )
        defaultValue


type alias Model itemType itemStruct =
    { body : ModelBody itemType itemStruct
    , defaultValue : itemType
    }


type ModelBody itemType itemStruct
    = SelectionModel
        { list : List itemType
        , selection : Selection
        }
    | EditModel
        { first : List itemType
        , editing : itemType
        , edition : itemStruct
        , last : List itemType
        }
    | DragModel
        { caret : Maybe Int
        , list : List itemType
        , selectionList : List itemType
        , origin : Int
        }


type Selection
    = EmptySelection
    | SingleSelection Int


type Msg itemType itemStruct
    = NoOp
    | Deselect
    | SingleSelect Int
    | Editing (itemType -> itemStruct) Int
    | Edition itemStruct
    | CommitEdit (itemStruct -> Result String itemType)
    | Drag Int
    | DragOutOfBounds
    | Drop


update : Msg itemType itemStruct -> Model itemType itemStruct -> Model itemType itemStruct
update msg model =
    case model.body of
        SelectionModel selectionModel ->
            let
                { list, selection } =
                    selectionModel
            in
                case msg of
                    Deselect ->
                        applyDeselect model list

                    SingleSelect i ->
                        applySingleSelect i model list

                    Editing editionStart i ->
                        applyEditing editionStart
                            i
                            model
                            list

                    Edition value ->
                        applyEdition value model list

                    Drag i ->
                        case selection of
                            SingleSelection j ->
                                applyDragFromSelection i j list model

                            EmptySelection ->
                                applyDragFromSelection i i list model

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

                _ ->
                    model

        DragModel dragModel ->
            let
                { caret, list, selectionList, origin } =
                    dragModel
            in
                case msg of
                    Drag j ->
                        { model | body = DragModel { dragModel | caret = Just j } }

                    DragOutOfBounds ->
                        { model | body = DragModel { dragModel | caret = Nothing } }

                    Drop ->
                        { model
                            | body =
                                case caret of
                                    Nothing ->
                                        SelectionModel
                                            { selection = SingleSelection origin
                                            , list =
                                                (List.take origin list)
                                                    ++ selectionList
                                                    ++ (List.drop origin list)
                                            }

                                    Just i ->
                                        SelectionModel
                                            { selection = SingleSelection i
                                            , list =
                                                (List.take i list)
                                                    ++ selectionList
                                                    ++ (List.drop i list)
                                            }
                        }

                    _ ->
                        model


applyDragFromSelection : Int -> Int -> List t -> Model t s -> Model t s
applyDragFromSelection i j list model =
    { model
        | body =
            DragModel
                { caret = Just i
                , selectionList =
                    [ (list
                        |> List.drop j
                        |> List.head
                        |> Maybe.withDefault model.defaultValue
                      )
                    ]
                , list = (List.take j list) ++ (List.drop (j + 1) list)
                , origin = i
                }
    }


applyDeselect : Model itemType itemStruct -> List itemType -> Model itemType itemStruct
applyDeselect model list =
    { model
        | body =
            SelectionModel
                { list = list
                , selection = EmptySelection
                }
    }


applySingleSelect : Int -> Model itemType itemStruct -> List itemType -> Model itemType itemStruct
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


applyEditing : (itemType -> itemStruct) -> Int -> Model itemType itemStruct -> List itemType -> Model itemType itemStruct
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


applyEdition : itemStruct -> Model itemType itemStruct -> List itemType -> Model itemType itemStruct
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


list : Model itemType itemStruct -> List itemType
list model =
    case model.body of
        SelectionModel { list, selection } ->
            list

        EditModel editModel ->
            listFromEditModel editModel

        DragModel { origin, selectionList, list } ->
            (list
                |> List.take origin
            )
                ++ selectionList
                ++ (list
                        |> List.drop origin
                   )


listFromEditModel { first, editing, last } =
    first ++ (editing :: last)


selection : Model itemType itemStruct -> Selection
selection model =
    case model.body of
        SelectionModel { selection } ->
            selection

        EditModel _ ->
            EmptySelection

        DragModel { origin } ->
            SingleSelection origin


inSelection : Selection -> Int -> Bool
inSelection selection i =
    case selection of
        EmptySelection ->
            False

        SingleSelection j ->
            i == j


type alias ItemViewType itemType itemStruct htmlMsg =
    { default : Int -> itemType -> htmlMsg
    , selected : Int -> itemType -> htmlMsg
    , editing : itemStruct -> Int -> itemType -> htmlMsg
    , caret : Int -> htmlMsg
    }


view : ItemViewType itemType itemStruct htmlMsg -> Model itemType itemStruct -> List htmlMsg
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

        DragModel { caret, list, selectionList, origin } ->
            case caret of
                Nothing ->
                    list
                        |> List.indexedMap itemViewType.default

                Just i ->
                    (list
                        |> List.take i
                        |> List.indexedMap itemViewType.default
                    )
                        ++ ((itemViewType.caret i)
                                :: (list
                                        |> List.drop i
                                        |> List.indexedMap
                                            ((addMap i)
                                                itemViewType.default
                                            )
                                   )
                           )


addMap : Int -> (Int -> itemType -> itemStruct) -> Int -> itemType -> itemStruct
addMap offset itemView i value =
    itemView (i + offset) value


itemSelectionView : ItemViewType itemType itemStruct htmlMsg -> Selection -> Int -> itemType -> htmlMsg
itemSelectionView itemViewType selection i value =
    if i |> inSelection selection then
        itemViewType.selected i value
    else
        itemViewType.default i value
