module Playground

open Fable.React
open Fable.React.Props
open Feliz

[<ReactComponent>]
let View () =
    B.Container [ Css.M5 ] [
        DynamicForm.Root()
    ]

ReactDom.render (View(), Browser.Dom.document.getElementById ("elmish-app"))
