module ResultCE

type ResultBuilder() =
    member _.Bind(x, f) =
        match x with
        | Error er -> Error er
        | Ok a -> f a

    member _.Return(x) = Ok x

    member _.ReturnFrom(x) = x

let result = ResultBuilder()
