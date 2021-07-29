module PetStoreView

open System.Collections.Generic
open Fable.React
open Fable.React.Props
open PetStore
open PetStore.Types

let petStoreUri = "https://petstore3.swagger.io/api/v3"

type Model<'T, 'Col when 'Col :> IComparer<'T>> =
    { Items: 'T list
      SortedAndFilteredItems: 'T list
      Sort: 'Col * SortDirection
      Filter: 'T -> bool }
    static member Init(sortCol, ?sortDir, ?filter) =
        { Items = []
          SortedAndFilteredItems = []
          Sort = sortCol, defaultArg sortDir Ascending
          Filter = defaultArg filter (fun _ -> true) }

type Msg<'T, 'Col> =
    | UpdateItems of 'T list
    | SortAndFilterItems
    | UpdateSort of 'Col * SortDirection
    | UpdateFilter of ('T -> bool)

let update msg model =
    let sortAndFilter model =
        let sortCol, sortDir = model.Sort

        let comparer =
            let comparer = sortCol :> IComparer<_>

            match sortDir with
            | Ascending -> (fun x y -> comparer.Compare(x, y))
            | Descending -> (fun x y -> comparer.Compare(x, y) * -1)

        { model with
              SortedAndFilteredItems =
                  model.Items
                  |> List.filter model.Filter
                  |> List.sortWith comparer }

    match msg with
    | UpdateItems items -> { model with Items = items } |> sortAndFilter
    | SortAndFilterItems -> sortAndFilter model
    | UpdateSort (col, dir) -> { model with Sort = col, dir } |> sortAndFilter
    | UpdateFilter filter -> { model with Filter = filter } |> sortAndFilter

type Pet with
    member this.idAsString =
        this.id |> Option.defaultValue 0L |> string

    member this.categoryAsString =
        this.category
        |> Option.bind (fun c -> c.name)
        |> Option.defaultValue ""

    member this.statusAsString =
        this.status
        |> Option.map string
        |> Option.defaultValue ""


type Col =
    | Name
    | Category
    interface IComparer<Pet> with
        member this.Compare(x, y) =
            match this with
            | Name -> compare x.name y.name
            | Category -> compare x.categoryAsString y.categoryAsString

type Model = Model<Pet, Col>

let petStore = PetStoreClient(petStoreUri)

let getPets dispatch =
    async {
        match! petStore.findPetsByStatus (status = string PetStatus.Available) with
        | FindPetsByStatus.BadRequest _ -> ()
        | FindPetsByStatus.OK (pets) -> UpdateItems pets |> dispatch
    }

let makeFilter (filter: {| name: string; category: string |}) =
    let nameFilter =
        let name = filter.name.Trim().ToLower()

        if name.Length > 0 then
            Some(fun (pet: Pet) -> pet.name.ToLower().Contains(name))
        else
            None

    let categoryFilter =
        let category = filter.category.Trim().ToLower()

        if category.Length > 0 then
            Some(fun (pet: Pet) -> pet.categoryAsString.ToLower().Contains(category))
        else
            None

    match nameFilter, categoryFilter with
    | Some f, None
    | None, Some f -> f
    | Some f1, Some f2 -> fun x -> f1 x && f2 x
    | None, None -> fun _ -> true

[<Feliz.ReactComponent>]
let Root () =
    let filter =
        Hooks.useState {| name = ""; category = "" |}

    let state =
        Hooks.useStateLazy (fun _ -> Model.Init(Name))

    let model = state.current
    let dispatch msg = state.update (update msg)

    let updateFilter f =
        filter.update
            (fun v ->
                let filter = f v
                makeFilter filter |> UpdateFilter |> dispatch
                filter)

    Hooks.useEffect (
        (fun _ ->
            if model.Items.IsEmpty then
                getPets dispatch |> Async.StartImmediate),
        [||]
    )

    // let rows =
    //     match model.Items with
    //     | [] -> B.p [] "No pets :("
    //     | pets ->
    //         B.ul [] [
    //             for pet in model.Items -> B.li [] [ B.p [] pet.name ]
    //         ]

    let sortableHeader col txt =
        let sortCol, sortDir = model.Sort

        B.ThSortable
            txt
            (if sortCol = col then
                 Some sortDir
             else
                 None)
            (fun dir -> UpdateSort(col, dir) |> dispatch)


    let rows =
        B.Table
            [ Css.TableStriped
              Css.TableHover
              Css.Border ]
            [ sortableHeader Name "Name"
              sortableHeader Category "Category"
              th [] [ str "status" ] ]
            [ for pet in model.SortedAndFilteredItems do
                  tr [ Key pet.idAsString ] [
                      td [] [ str pet.name ]
                      td [] [ str pet.categoryAsString ]
                      td [] [ str pet.statusAsString ]
                  ] ]

    fragment [] [
        B.div [ Css.DFlex; Css.Mb3 ] [
            B.Input(
                classes = [ Css.Me2 ],
                label_ = "Name",
                value = filter.current.name,
                onChange = fun v -> updateFilter (fun f -> {| f with name = v |})
            )
            B.Input(
                label_ = "Category",
                value = filter.current.category,
                onChange = fun v -> updateFilter (fun f -> {| f with category = v |})
            )
        ]
        rows
    ]
