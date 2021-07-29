namespace global

open Fable.React
open Fable.React.Props
open Zanaptak.TypedCssClasses

type Css = CssClasses<"https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/css/bootstrap.css", Naming.PascalCase>

type Fa =
    CssClasses<"https://cdn.jsdelivr.net/npm/@fortawesome/fontawesome-free@5.15.3/css/fontawesome.css", Naming.PascalCase>

type SortDirection =
    | Ascending
    | Descending

type B =
    static member classes(classes: string list) = classes |> String.concat " " |> Class // TODO: Trim? Filter empty? Omit if empty list?

    static member data(key: string, value: obj) =
        HTMLAttr.Custom("data-bs-" + key, value)

    static member section (classes: string list) (children: ReactElement list) = section [ B.classes classes ] children
    static member div (classes: string list) (children: ReactElement list) = div [ B.classes classes ] children

    static member form onSubmit (classes: string list) (children: ReactElement list) =
        form
            [ OnSubmit
                (fun ev ->
                    ev.preventDefault ()
                    onSubmit ())
              B.classes classes ]
            children

    static member span (classes: string list) (children: ReactElement list) = span [ B.classes classes ] children
    static member ul (classes: string list) (children: ReactElement list) = ul [ B.classes classes ] children
    static member li (classes: string list) (children: ReactElement list) = li [ B.classes classes ] children
    static member a (classes: string list) (children: ReactElement list) = a [ B.classes classes ] children
    static member table classes (children: ReactElement list) = table [ B.classes classes ] children
    static member thead classes (children: ReactElement list) = thead [ B.classes classes ] children
    static member tbody classes (children: ReactElement list) = tbody [ B.classes classes ] children
    static member th classes (children: ReactElement list) = th [ B.classes classes ] children
    static member tr classes (children: ReactElement list) = tr [ B.classes classes ] children
    static member td classes (children: ReactElement list) = td [ B.classes classes ] children
    static member button classes (children: ReactElement list) = button [ B.classes classes ] children
    static member nav classes (children: ReactElement list) = nav [ B.classes classes ] children

    static member i(classes: string list) = i [ B.classes classes ] []
    static member hr(classes: string list) = hr [ B.classes classes ]

    static member h1 classes (text: string) = h1 [ B.classes classes ] [ str text ]
    static member h2 classes (text: string) = h2 [ B.classes classes ] [ str text ]
    static member h3 classes (text: string) = h3 [ B.classes classes ] [ str text ]
    static member h4 classes (text: string) = h4 [ B.classes classes ] [ str text ]
    static member h5 classes (text: string) = h5 [ B.classes classes ] [ str text ]
    static member p classes (text: string) = p [ B.classes classes ] [ str text ]

    static member strong classes (text: string) =
        strong [ B.classes classes ] [
            str text
        ]

    static member label classes (text: string) =
        label [ B.classes classes ] [ str text ]

    static member Fa classes = B.i (Fa.Fa :: classes)
    static member Fab classes = B.i (Fa.Fab :: classes)
    static member Fal classes = B.i (Fa.Fal :: classes)
    static member Far classes = B.i (Fa.Far :: classes)
    static member Fas classes = B.i (Fa.Fas :: classes)

    // static member Title (text: string) = B.h1 [ Css.Title ] text
    static member Container classes (children: ReactElement list) =
        B.div (Css.Container :: classes) children

    static member Row classes (children: ReactElement list) = B.div (Css.Row :: classes) children
    static member Col classes (children: ReactElement list) = B.div (Css.Col :: classes) children
    static member FlexRow classes (children: ReactElement list) = B.div (Css.DFlex :: classes) children

    static member TableWithBodyClasses classes bodyClasses (headers: ReactElement list) (rows: ReactElement list) =
        B.div [ Css.TableResponsive ] [
            table [ B.classes (Css.Table :: classes) ] [
                thead [] [ tr [] headers ]
                tbody [ B.classes bodyClasses ] rows
            ]
        ]

    static member Table classes (headers: ReactElement list) (rows: ReactElement list) =
        B.TableWithBodyClasses classes [] headers rows

    static member ThSortable txt sort dispatch =
        th [ Class "clickable"
             OnClick
                 (fun _ ->
                     match sort with
                     | Some Ascending -> dispatch Descending
                     | Some Descending
                     | None -> dispatch Ascending) ] [

            B.div [ Css.DFlex; Css.AlignItemsCenter ] [
                B.span [ Css.FlexGrow1 ] [ str txt ]
                match sort with
                | None -> ()
                | Some dir ->
                    B.Fa [
                        if dir = Descending then
                            Fa.FaSortDown
                        else
                            Fa.FaSortUp
                    ]
            ]
        ]

    static member Button(?classes: string list, ?onClick: unit -> unit, ?disabled: bool, ?text: string) =
        button [ B.classes (Css.Btn :: (defaultArg classes []))
                 Disabled(defaultArg disabled false)
                 Type "button"
                 OnClick
                     (fun ev ->
                         ev.preventDefault ()

                         match onClick with
                         | Some f -> f ()
                         | None -> ()) ] [
            str (defaultArg text "")
        ]

    static member Checkbox(text: string, isChecked: bool, onChecked: bool -> unit, ?disabled: bool) =

        let id = System.Guid.NewGuid().ToString()

        B.div [ Css.FormCheck ] [
            input [
                Type "checkbox"
                Class Css.FormCheckInput
                Id id
                Checked isChecked
                OnChange(fun ev -> onChecked ev.Checked)
                Disabled(defaultArg disabled false)
            ]
            label [ Class Css.FormCheckLabel; HtmlFor id ] [
                str text
            ]
        ]

    static member Input(label_, value, onChange, ?classes, ?type_, ?disabled) =
        B.div
            (defaultArg classes [])
            [ label [ Class Css.FormLabel ] [
                str label_
              ]
              input [
                  Type(defaultArg type_ "text")
                  B.classes [ Css.FormControl ]
                  OnChange(fun ev -> onChange ev.Value)
                  Disabled(defaultArg disabled false)
                  Value value
              ] ]

    static member Nav classes (children: ReactElement list) = B.ul (Css.Nav :: classes) children
    static member NavItem classes (children: ReactElement list) = B.li (Css.NavItem :: classes) children
    static member NavLink classes (children: ReactElement list) = B.a (Css.NavLink :: classes) children

    static member Pills classes (children: ReactElement list) =
        B.ul ($"{Css.Nav} {Css.NavPills}" :: classes) children

    static member Tabs tabs content =
        div [] [
            ul
                [ Role "tablist"
                  B.classes [ Css.Nav; Css.NavTabs ] ]
                tabs
            B.div [ Css.TabContent ] [
                div
                    [ Role "tabpanel"
                      B.classes [ Css.TabPane; Css.Active ] ]
                    content
            ]
        ]

    static member Tab isActive onClick children =
        li [ Role "presentation"
             Class Css.NavItem ] [
            a
                [ Href "#"
                  Class(
                      "nav-link tab"
                      + if isActive then " active" else ""
                  )
                  OnClick
                      (fun ev ->
                          ev.preventDefault ()
                          onClick ()) ]
                children
        ]

    static member Toast msg =
        div [ B.classes [
                  Css.PositionFixed
                  Css.Bottom0
                  Css.End0
                  Css.P3
              ]
              Style [ ZIndex 1000 ] ] [

            B.div [ Css.Toast
                    Css.AlignItemsCenter
                    Css.TextWhite
                    Css.BgPrimary
                    Css.Border0 ] [
                B.div [ Css.DFlex ] [
                    B.div [ Css.ToastBody ] [ str msg ]
                ]
            ]
        ]

    static member Carousel id (images: {| src: string; caption: string |} list) =
        div [ Id id
              B.classes [ Css.Carousel; "slide" ]
              B.data ("ride", "carousel") ] [
            B.div [ Css.CarouselIndicators ] [
                button [ Type "button"
                         B.data ("target", "#" + id)
                         B.data ("slide-to", 0)
                         Class Css.Active ] []
                button [ Type "button"
                         B.data ("target", "#" + id)
                         B.data ("slide-to", 1) ] []
                button [ Type "button"
                         B.data ("target", "#" + id)
                         B.data ("slide-to", 2) ] []
            ]
            B.div
                [ Css.CarouselInner ]
                (images
                 |> List.mapi
                     (fun idx image ->
                         div [ B.classes [
                                   Css.CarouselItem
                                   if idx = 0 then Css.Active
                               ] ] [
                             img [
                                 Src image.src
                                 Alt image.caption
                                 B.classes [ Css.DBlock; Css.W100 ]
                             ]
                             B.div [ Css.CarouselCaption
                                     Css.DNone
                                     Css.DMdBlock ] [
                                 B.p [] image.caption
                             ]
                         ]))

            button [ Type "button"
                     B.data ("target", "#" + id)
                     B.data ("slide-to", "prev") ] [
                B.span [ Css.CarouselControlPrevIcon ] []
            ]
            button [ Type "button"
                     B.data ("target", "#" + id)
                     B.data ("slide-to", "next") ] [
                B.span [ Css.CarouselControlNextIcon ] []
            ]
        ]
