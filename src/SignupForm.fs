module SignupForm

open Fable.Form.Simple
open Fable.Form.Simple.Bootstrap

/// Validated user
type User =
    { Email: string
      Password: string
      Name: string
      MakePublic: bool }

type ServerError = { Value: string; Error: string }

type ServerErrors = { Email: ServerError option }

type FormValues =
    { Email: string
      Password: string
      RepeatPassword: string
      Name: string
      MakePublic: bool
      ServerErrors: ServerErrors }

module Validation =
    open System.Text.RegularExpressions

    let emailRegex =
        Regex("""^([a-z0-9äöü_\.\+-]+)@([\da-zöäü\.-]+)\.([a-z\.]{2,6})$""", RegexOptions.IgnoreCase)

    let tryParseEmail (value: string) =
        if emailRegex.IsMatch(value) then
            Ok value
        else
            Error "Enter a valid email"

    let tryParseName (value: string) = Ok value

    let tryParsePassword (value: string) =
        let minLen = 6

        if value.Length < minLen then
            Error $"Password must have at least {minLen} characters"
        else
            Ok value

    let tryUser (form: FormValues) =
        ResultCE.result {
            let! email = tryParseEmail form.Email
            let! name = tryParseName form.Name
            let! password = tryParsePassword form.Password

            if password <> form.RepeatPassword then
                return! Error "Passwords are not equal"
            else
                return
                    { Email = email
                      Password = password
                      Name = name
                      MakePublic = form.MakePublic }
        }


type Model =
    | FillingForm of Form.View.Model<FormValues>
    | Success of User

type Msg =
    | FormChanged of Form.View.Model<FormValues>
    | Signup of User
    | ResetTheDemo

let init () =
    { Email = ""
      Password = ""
      RepeatPassword = ""
      Name = ""
      MakePublic = false
      ServerErrors = { Email = None } }
    |> Form.View.idle // By default, set the form in idle mode
    |> FillingForm

let update (msg: Msg) (model: Model) =
    match msg with
    | FormChanged formModel ->
        match model with
        | FillingForm _ -> FillingForm formModel
        | _ -> model

    // Dummy implementation, here we should call the server
    | Signup validUser ->
        match model with
        | FillingForm form -> Success validUser
        // { form.Values with
        //       ServerErrors =
        //           { Email =
        //                 Some
        //                     { Value = form.Values.Email
        //                       Error = "This email is already registered" } } }
        // |> Form.View.idle
        // |> FillingForm
        | _ -> model

    | ResetTheDemo -> init ()

let private form: Form.Form<FormValues, Msg> =
    let onSubmit email name password makePublic =
        Signup
            { Email = email
              Password = password
              Name = name
              MakePublic = makePublic }

    let emailField =
        Form.Make.FieldEmail(
            "Email",
            parse = Validation.tryParseEmail,
            get = (fun vs -> vs.Email),
            update = (fun v vs -> { vs with Email = v }),
            serverError =
                (fun values ->
                    match values.ServerErrors.Email with
                    | Some er when er.Value = values.Email -> Some er.Error
                    | _ -> None)
        )

    let nameField =
        Form.Make.FieldText(
            "Name",
            parse = Validation.tryParseName,
            get = (fun vs -> vs.Name),
            update = (fun v vs -> { vs with Name = v })
        )

    let passwordField =
        Form.Make.FieldPassword(
            "Password",
            parse = Validation.tryParsePassword,
            get = (fun vs -> vs.Password),
            update = (fun v vs -> { vs with Password = v })
        )

    let repeatPasswordField =
        Form.meta
            (fun values ->
                Form.Make.FieldPassword(
                    "Password",
                    parse =
                        (fun value ->
                            if value = values.Password then
                                Ok value
                            else
                                Error "The passwords do not match"),
                    get = (fun vs -> vs.RepeatPassword),
                    update = (fun v vs -> { vs with RepeatPassword = v })
                ))

    let makePublicField =
        Form.Make.Checkbox(
            "Make your profile public?",
            get = (fun vs -> vs.MakePublic),
            update = (fun v vs -> { vs with MakePublic = v })
        )

    Form.succeed onSubmit
    |> Form.append emailField
    |> Form.append nameField
    |> Form.append (
        Form.succeed (fun password _ -> password)
        |> Form.append passwordField
        |> Form.append repeatPasswordField
        |> Form.group
    )
    |> Form.append makePublicField

let view (model: Model) dispatch =
    match model with
    | FillingForm values ->
        Form.View.asHtml
            { Dispatch = dispatch
              OnChange = FormChanged
              Action = "Sign up"
              Validation = Form.View.ValidateOnBlur }
            form
            values

    | Success user -> B.h2 [] $"Congratulations! Now {user.Name} is a member of the team!"

open Fable.React

[<Feliz.ReactComponent>]
let Root () =
    let state = Hooks.useStateLazy (fun () -> init ())

    let dispatch msg = state.update (update msg)

    view state.current dispatch
