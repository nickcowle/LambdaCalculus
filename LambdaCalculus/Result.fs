namespace LambdaCalculus

type Result<'s, 'f> =
| Success of 's
| Failure of 'f

module Result =

    let force =
        function
        | Success s -> s
        | Failure f -> failwithf "Result was failure: %A" f
