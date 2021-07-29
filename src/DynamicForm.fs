module DynamicForm

open Fable.React
open Elmish
open Fable.Form.Simple
open Fable.Form.Simple.Bootstrap

type UserType =
    | Student
    | Teacher

type Values =
    { UserType: UserType
      Name: string
      Subject: string }

type Model =
    | FillingForm of Form.View.Model<Values>
    | CreatedTeacher of string * string
    | CreatedStudent of string

type Msg =
    | FormChanged of Form.View.Model<Values>
    | NewStudent of string
    | NewTeacher of string * string
    | ResetDemo

let init () =
    { UserType = Student
      Name = ""
      Subject = "" }
    |> Form.View.idle
    |> FillingForm

let update (msg: Msg) (model: Model) =
    match msg with
    | FormChanged newModel ->
        match model with
        | FillingForm _ -> FillingForm newModel
        | CreatedStudent _
        | CreatedTeacher _ -> model

    | NewStudent name ->
        match model with
        | FillingForm _ -> CreatedStudent name
        | CreatedStudent _
        | CreatedTeacher _ -> model

    | NewTeacher (name, subject) ->
        match model with
        | FillingForm _ -> CreatedTeacher(name, subject)
        | CreatedStudent _
        | CreatedTeacher _ -> model

    | ResetDemo -> init ()

let private studentForm =
    Form.succeed NewStudent
    |> Form.append (Form.Make.FieldText("Name", get = (fun vs -> vs.Name), update = (fun v vs -> { vs with Name = v })))
    |> Form.section "Student"

let private teacherForm =
    Form.succeed (fun name subject -> NewTeacher(name, subject))
    |> Form.append (Form.Make.FieldText("Name", get = (fun vs -> vs.Name), update = (fun v vs -> { vs with Name = v })))
    |> Form.append (
        Form.Make.FieldText("Subject", get = (fun vs -> vs.Subject), update = (fun v vs -> { vs with Subject = v }))
    )
    |> Form.section "Teacher"

let private form: Form.Form<Values, Msg> =
    Form.Make.Select("Type", get = (fun vs -> vs.UserType), update = (fun v vs -> { vs with UserType = v }))
    |> Form.andThen
        (function
        | Student -> studentForm
        | Teacher -> teacherForm)

let private renderResultView dispatch (messageBody: ReactElement) =
    B.div [] [
        B.div [ Css.Card; Css.Mb3 ] [
            messageBody
        ]
        B.Button(text = "Reset the demo", onClick = (fun _ -> dispatch ResetDemo), classes = [ Css.BtnPrimary ])
    ]

let view (model: Model) (dispatch: Dispatch<Msg>) =
    match model with
    | FillingForm values ->
        let actionText =
            match values.Values.UserType with
            | Student -> "Create student"
            | Teacher -> "Create teacher"
        // | Assistant -> "Create assistant"

        Form.View.asHtml
            { Dispatch = dispatch
              OnChange = FormChanged
              Action = actionText
              Validation = Form.View.ValidateOnBlur }
            form
            values

    | CreatedStudent name ->
        B.div [ Css.ToastBody ] [
            B.p [] "A new student has been created"
            br []
            p [] [ str "Name: "; B.strong [] name ]
        ]
        |> renderResultView dispatch

    | CreatedTeacher (name, subject) ->
        B.div [ Css.ToastBody ] [
            B.p [] "A new teacher has been created"
            p [] [ str "Name: "; B.strong [] name ]
            p [] [
                str "Teaching: "
                B.strong [] subject
            ]
        ]
        |> renderResultView dispatch


[<Feliz.ReactComponent>]
let Root () =
    let state = Hooks.useStateLazy (fun () -> init ())

    let dispatch msg = state.update (update msg)

    view state.current dispatch
