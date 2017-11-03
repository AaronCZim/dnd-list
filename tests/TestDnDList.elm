module TestDnDList exposing (..)

import Expect
import Test exposing (Test, describe, test)
import DnDList exposing (..)


suite : Test
suite =
    describe "DnDList Tests"
        [ test "SingleSelect 0 on empty list has no effect" <|
            \() ->
                (init0 |> update (SingleSelect 0))
                    |> Expect.equal
                        init0
        , test "SingleSelect 0 on [0,1,2] works" <|
            \() ->
                (init3 |> update (SingleSelect 0))
                    |> .body
                    |> Expect.equal
                        (SelectionModel
                            { list = [ 0, 1, 2 ]
                            , selection = (SingleSelection 0)
                            }
                        )
        , test "Deselect on [0,1,2] SingleSelect 0" <|
            \() ->
                (({ init3
                    | body =
                        SelectionModel
                            { list = [ 0, 1, 2 ]
                            , selection = (SingleSelection 0)
                            }
                  }
                 )
                    |> update Deselect
                )
                    |> .body
                    |> Expect.equal
                        (SelectionModel
                            { list = [ 0, 1, 2 ]
                            , selection = EmptySelection
                            }
                        )
        , test "Editing 0 on init3" <|
            \() ->
                (init3 |> update (Editing 0))
                    |> .body
                    |> Expect.equal
                        (EditModel
                            { first = []
                            , editing = 0
                            , last = [ 1, 2 ]
                            }
                        )
        , test "Editing 1 on init3" <|
            \() ->
                (init3 |> update (Editing 1))
                    |> .body
                    |> Expect.equal
                        (EditModel
                            { first = [ 0 ]
                            , editing = 1
                            , last = [ 2 ]
                            }
                        )
        , test "Editing 2 on init3" <|
            \() ->
                (init3 |> update (Editing 2))
                    |> .body
                    |> Expect.equal
                        (EditModel
                            { first = [ 0, 1 ]
                            , editing = 2
                            , last = []
                            }
                        )
        , test "Edit 0 -1 on init3" <|
            \() ->
                (init3 |> update (Edit 0 -1))
                    |> .body
                    |> Expect.equal
                        (EditModel
                            { first = []
                            , editing = -1
                            , last = [ 1, 2 ]
                            }
                        )
        , test "Edit 0 -1 on Editing 0 init3" <|
            \() ->
                (init3 |> update (Editing 0) |> update (Edit 0 -1))
                    |> .body
                    |> Expect.equal
                        (EditModel
                            { first = []
                            , editing = -1
                            , last = [ 1, 2 ]
                            }
                        )
        , test "Edit 0 -1 on Editing 2 init3" <|
            \() ->
                (init3 |> update (Editing 2) |> update (Edit 0 -1))
                    |> .body
                    |> Expect.equal
                        (EditModel
                            { first = []
                            , editing = -1
                            , last = [ 1, 2 ]
                            }
                        )
        , test "SingleSelect 1 on Editing 0" <|
            \() ->
                (init3 |> update (Editing 0) |> update (SingleSelect 1))
                    |> .body
                    |> Expect.equal
                        (SelectionModel
                            { list = [ 0, 1, 2 ]
                            , selection = SingleSelection 1
                            }
                        )
        ]


init0 =
    init [] 0


init3 : Model Int
init3 =
    init [ 0, 1, 2 ] 0
