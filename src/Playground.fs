module Playground

open Fable.React
open Fable.React.Props

[<Feliz.ReactComponent>]
let View() =
    B.section [ Css.ContainerFluid ] [
        Page.Page()
    ]

ReactDom.render(View(), Browser.Dom.document.getElementById("elmish-app"))