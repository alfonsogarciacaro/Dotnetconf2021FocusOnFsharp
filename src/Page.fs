module Page

open Fable.React
open Fable.React.Props

type Model =
    { Count: int }
    static member Initial =
        { Count = 0 }

type Msg =
    | Add of int

let update msg model =
    match msg with
    | Add v -> { model with Count = model.Count + v }

[<Feliz.ReactComponent>]
let Page() =
    let state = Hooks.useState Model.Initial
    let update msg = state.update(update msg)

    B.Container [] [
        B.h1 [Css.My3] $"Count: %i{state.current.Count}"
        B.Button(
            classes=[Css.BtnPrimary],
            onClick=(fun _ -> update (Add 4)),
            text="+"
        )
        B.Button(
            classes=[Css.BtnDanger; Css.Ms5],
            onClick=(fun _ -> update (Add -10)),
            text="-"
        )
    ]
