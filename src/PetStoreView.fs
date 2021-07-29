module PetStoreView

open System.Collections.Generic
open Fable.React
open Fable.React.Props
open PetStore
open PetStore.Types

let petStoreUri = "https://petstore3.swagger.io/api/v3"
let petStore = PetStoreClient(petStoreUri)

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



module ListView =
    type IFilter<'T> =
        abstract MakeFilter : unit -> ('T -> bool)

    type Model<'T, 'Col, 'Filter when 'Col :> IComparer<'T> and 'Filter :> IFilter<'T>> =
        { Items: 'T list
          SortedAndFilteredItems: 'T list
          Sort: 'Col * SortDirection
          Filter: 'Filter }
        static member Init(sortCol, sortDir, filter) =
            { Items = []
              SortedAndFilteredItems = []
              Sort = sortCol, sortDir
              Filter = filter }

    type Msg<'T, 'Col, 'Filter> =
        | UpdateItems of 'T list
        | UpdateSort of 'Col * SortDirection
        | UpdateFilter of 'Filter

    let update msg model =
        let sortAndFilter model =
            let sortCol, sortDir = model.Sort

            let comparer =
                let comparer = sortCol :> IComparer<_>

                match sortDir with
                | Ascending -> (fun x y -> comparer.Compare(x, y))
                | Descending -> (fun x y -> comparer.Compare(x, y) * -1)

            let filter =
                (model.Filter :> IFilter<_>).MakeFilter()

            { model with
                  SortedAndFilteredItems =
                      model.Items
                      |> List.filter filter
                      |> List.sortWith comparer }

        match msg with
        | UpdateItems items -> { model with Items = items } |> sortAndFilter
        | UpdateSort (col, dir) -> { model with Sort = col, dir } |> sortAndFilter
        | UpdateFilter filter -> { model with Filter = filter } |> sortAndFilter



open ListView

type Col =
    | Name
    | Category
    interface IComparer<Pet> with
        member this.Compare(x, y) =
            match this with
            | Name -> compare x.name y.name
            | Category -> compare x.categoryAsString y.categoryAsString

type Filter =
    { Name: string
      Category: string }

    static member Empty = { Name = ""; Category = "" }

    interface IFilter<Pet> with
        member this.MakeFilter() =
            let nameFilter =
                let name = this.Name.Trim().ToLower()

                if name.Length > 0 then
                    Some(fun (pet: Pet) -> pet.name.ToLower().Contains(name))
                else
                    None

            let categoryFilter =
                let category = this.Category.Trim().ToLower()

                if category.Length > 0 then
                    Some(fun (pet: Pet) -> pet.categoryAsString.ToLower().Contains(category))
                else
                    None

            match nameFilter, categoryFilter with
            | Some f, None
            | None, Some f -> f
            | Some f1, Some f2 -> fun x -> f1 x && f2 x
            | None, None -> fun _ -> true


type Model = Model<Pet, Col, Filter>

let getPets dispatch =
    async {
        match! petStore.findPetsByStatus (status = string PetStatus.Available) with
        | FindPetsByStatus.BadRequest _ -> ()
        | FindPetsByStatus.OK (pets) -> UpdateItems pets |> dispatch
    }

let sortableHeader model dispatch col txt =
    let sortCol, sortDir = model.Sort

    B.ThSortable
        txt
        (if sortCol = col then
             Some sortDir
         else
             None)
        (fun dir -> UpdateSort(col, dir) |> dispatch)


[<Feliz.ReactComponent>]
let Root () =
    let state =
        Hooks.useStateLazy (fun _ -> Model.Init(Name, Ascending, Filter.Empty))

    let model = state.current
    let dispatch msg = state.update (update msg)

    Hooks.useEffect (
        (fun _ ->
            if model.Items.IsEmpty then
                getPets dispatch |> Async.StartImmediate),
        [||]
    )

    match model.Items with
    | [] -> B.p [] "No pets :("
    | pets ->
        B.ul [] [
            for pet in pets -> B.li [] [ B.p [] pet.name ]
        ]























// let rows =
//     B.Table
//         [ Css.TableStriped
//           Css.TableHover
//           Css.Border ]
//         [ th [] [ str "Name" ]
//           th [] [ str "Category" ]
//           th [] [ str "Status" ] ]
//         [ for pet in model.SortedAndFilteredItems do
//               tr [ Key pet.idAsString ] [
//                   td [] [ str pet.name ]
//                   td [] [ str pet.categoryAsString ]
//                   td [] [ str pet.statusAsString ]
//               ] ]

// rows


// fragment [] [
//     B.div [ Css.DFlex; Css.Mb3 ] [
//         B.Input(
//             classes = [ Css.Me2 ],
//             label_ = "Name",
//             value = model.Filter.Name,
//             onChange =
//                 fun v ->
//                     { model.Filter with Name = v }
//                     |> UpdateFilter
//                     |> dispatch
//         )
//         B.Input(
//             label_ = "Category",
//             value = model.Filter.Category,
//             onChange =
//                 fun v ->
//                     { model.Filter with Category = v }
//                     |> UpdateFilter
//                     |> dispatch
//         )
//     ]
//     rows
// ]
