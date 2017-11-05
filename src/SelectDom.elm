module SelectDom exposing (..)

import Native.SelectDom
import Task


type NotFoundError
    = NotFound String


focus : String -> Task.Task String ()
focus className =
    Native.SelectDom.focus className


select : String -> Task.Task String ()
select className =
    Native.SelectDom.select className
