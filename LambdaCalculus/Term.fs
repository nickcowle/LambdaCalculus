namespace LambdaCalculus

[<StructuredFormatDisplay("{AsString}")>]
type Term =
| Var of int
| App of Term * Term
| Lam of Term
with
    member __.AsString = "λ"

[<AutoOpen>]
module Constructors =

    let rec lams n t = match n with 0 -> t | _ -> Lam (lams (n - 1) t)

    let rec apps = List.reduce (fun t1 t2 -> App (t1, t2))

    let rec appVars = List.map Var >> apps

    let ($) t1 t2 = App (t1, t2)
