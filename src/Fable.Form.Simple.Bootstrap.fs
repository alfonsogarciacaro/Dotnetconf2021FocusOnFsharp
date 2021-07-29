namespace Fable.Form.Simple.Bootstrap

[<RequireQualifiedAccess>]
module Form =
    open Fable.Form.Base
    open Fable.Form.Simple

    type Make =
        static member Config
            (
                attributes: 'Atts,
                get: 'Values -> 'Input,
                update: 'Input -> 'Values -> 'Values,
                parse: 'Input -> Result<'Output, string>,
                ?serverError: 'Values -> string option
            ) : FieldConfig<_, _, _, _> =
            { Parser = parse
              Value = get
              Update = update
              Error = defaultArg serverError (fun _ -> None)
              Attributes = attributes }

        static member ConfigText(label, get, update, ?placeholder, ?parse, ?serverError) =
            let attributes: Field.TextField.Attributes =
                { Label = label
                  Placeholder = defaultArg placeholder "" }

            Make.Config(attributes, get, update, defaultArg parse Ok, ?serverError = serverError)

        static member FieldText(label, get, update, ?placeholder, ?parse, ?serverError) =
            Make.ConfigText(label, get, update, ?placeholder = placeholder, ?parse = parse, ?serverError = serverError)
            |> Form.textField

        static member FieldPassword(label, get, update, ?placeholder, ?parse, ?serverError) =
            Make.ConfigText(label, get, update, ?placeholder = placeholder, ?parse = parse, ?serverError = serverError)
            |> Form.passwordField

        static member FieldEmail(label, get, update, ?placeholder, ?parse, ?serverError) =
            Make.ConfigText(label, get, update, ?placeholder = placeholder, ?parse = parse, ?serverError = serverError)
            |> Form.emailField

        static member Checkbox(label, get, update, ?serverError) =
            Make.Config(
                { Field.CheckboxField.Attributes.Text = label },
                get,
                update,
                parse = Ok,
                ?serverError = serverError
            )
            |> Form.checkboxField

        static member Select<'T, 'Values when 'T: equality>
            (
                typ: System.Type,
                label: string,
                get: 'Values -> 'T,
                update: 'T -> 'Values -> 'Values,
                ?toString: 'T -> string,
                ?placeholder: string
            ) =
            let toString = toString |> Option.defaultValue string

            let values =
                Reflection.FSharpType.GetUnionCases(typ)
                |> Array.map (fun uci -> Reflection.FSharpValue.MakeUnion(uci, [||]) :?> 'T)

            let options =
                values
                |> Array.mapi (fun i v -> string i, toString v)
                |> Array.toList

            let attributes: Field.SelectField.Attributes =
                { Label = label
                  Placeholder = defaultArg placeholder ""
                  Options = options }

            let get formValues =
                let v: 'T = get formValues
                Array.findIndex ((=) v) values |> string

            let update (i: string) formValues = update values.[int i] formValues

            Make.Config(attributes, get, update, parse = (fun i -> Ok values.[int i]))
            |> Form.selectField

        static member inline Select<'T, 'Values when 'T: equality>(label, get, update, ?toString, ?placeholder) =
            Make.Select<'T, 'Values>(typeof<'T>, label, get, update, ?toString = toString, ?placeholder = placeholder)

    module View =

        open Fable.React
        open Fable.React.Props
        open Fable.Form
        open Fable.Form.Simple.Form.View

        let form
            ({ Dispatch = dispatch
               OnSubmit = onSubmit
               State = _state
               Action = action
               Fields = fields }: FormConfig<'Msg>)
            =
            match onSubmit with
            | Some msg ->
                B.form
                    (fun () -> dispatch msg)
                    []
                    [ yield! fields
                      button [ B.classes [ Css.Btn; Css.BtnPrimary ]
                               Type "submit" ] [
                          str action
                      ] ]
            | None -> form [] fields

        let inputField
            typ
            ({ Dispatch = dispatch
               OnChange = onChange
               OnBlur = onBlur
               Disabled = disabled
               Value = value
               Error = error
               ShowError = showError
               Attributes = attributes }: TextFieldConfig<'Msg>)
            =
            B.div [ Css.Mb3 ] [
                label [ Class Css.FormLabel ] [
                    str attributes.Label
                ]
                input [
                    Type typ
                    B.classes [
                        Css.FormControl
                        if showError && error.IsSome then
                            Css.IsInvalid
                    ]
                    OnChange(fun ev -> onChange ev.Value |> dispatch)
                    match onBlur with
                    | None -> ()
                    | Some onBlur -> OnBlur(fun _ -> dispatch onBlur)
                    Disabled disabled
                    Value value
                    Placeholder attributes.Placeholder
                ]
                match showError, error with
                | true, Some (Error.External msg)
                | true, Some (Error.ValidationFailed msg) -> B.p [ Css.InvalidFeedback ] msg
                | _ -> ()
            ]

        let checkboxField
            ({ Dispatch = dispatch
               OnChange = onChange
               OnBlur = onBlur
               Disabled = disabled
               Value = value
               Attributes = attributes }: CheckboxFieldConfig<'Msg>)
            =
            B.div [ Css.FormCheck; Css.Mb3 ] [
                input [
                    Type "checkbox"
                    Class Css.FormCheckInput
                    Checked value
                    OnChange(fun ev -> onChange ev.Checked |> dispatch)
                    match onBlur with
                    | None -> ()
                    | Some onBlur -> OnBlur(fun _ -> dispatch onBlur)
                    Disabled disabled
                ]
                label [ Class Css.FormCheckLabel ] [
                    str attributes.Text
                ]
            ]

        let selectField
            ({ Dispatch = dispatch
               OnChange = onChange
               OnBlur = onBlur
               Disabled = disabled
               Value = value
               Error = error
               ShowError = showError
               Attributes = attributes }: SelectFieldConfig<'Msg>)
            =
            B.div [ Css.Mb3 ] [
                label [ Class Css.FormLabel ] [
                    str attributes.Label
                ]
                select
                    [ B.classes [
                        Css.FormSelect
                        if showError && error.IsSome then
                            Css.IsInvalid
                      ]
                      Value value
                      Disabled disabled
                      OnChange(fun ev -> onChange ev.Value |> dispatch)
                      match onBlur with
                      | None -> ()
                      | Some onBlur -> OnBlur(fun _ -> dispatch onBlur) ]

                    (attributes.Options
                     |> List.map (fun (key, label) -> option [ Value key ] [ str label ]))
            ]

        let section (title: string) (fields: ReactElement list) =
            fieldset [] [
                legend [] [ str title ]
                yield! fields
            ]

        let group (fields: ReactElement list) =
            B.Row
                []
                (fields
                 |> List.map (fun field -> B.Col [] [ field ]))

        // Contract that we need to implement to support
        // all the feature offered by Fable.Simple.Form
        let htmlViewConfig<'Msg> : CustomConfig<'Msg> =
            { RadioField = fun _ -> B.p [] "TODO"
              FormList = fun _ -> B.p [] "TODO"
              FormListItem = fun _ -> B.p [] "TODO"
              TextAreaField = fun _ -> B.p [] "TODO"

              Group = group
              Section = section
              Form = form
              TextField = inputField "text"
              PasswordField = inputField "password"
              EmailField = inputField "email"
              CheckboxField = checkboxField
              SelectField = selectField }

        // Function which will be called by the consumer to render the form
        let asHtml (config: ViewConfig<'Values, 'Msg>) = custom htmlViewConfig config
