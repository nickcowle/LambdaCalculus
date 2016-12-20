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


[<StructuredFormatDisplay("{AsString}")>]
type TermI =
| VarI   of int
| AppI   of TermI * TermI
| LamI   of TermI
| IdentI of string
with
    member __.AsString = "λ"

[<AutoOpen>]
module ConstructorsI =

    let rec lamsI n t = match n with 0 -> t | _ -> LamI (lamsI (n - 1) t)

    let rec appsI = List.reduce (fun t1 t2 -> AppI (t1, t2))


[<StructuredFormatDisplay("{AsString}")>]
type TermIV =
| VarIV   of string
| AppIV   of TermIV * TermIV
| LamIV   of string * TermIV
| IdentIV of string
with
    member __.AsString = "λ"

[<AutoOpen>]
module ConstructorsIV =

    let rec lamsIV vs t = match vs with [] -> t | v::vs -> LamIV (v, lamsIV vs t)

    let rec appsIV = List.reduce (fun t1 t2 -> AppIV (t1, t2))

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module TermIV =

    let deBruijn =

        let rec resolveVariableNames context =
            function
            | VarIV s ->
                match List.tryFindIndex ((=) s) context with
                | None   -> failwithf "Could not find binding for variable %s" s
                | Some i -> VarI i
            | AppIV (t1, t2) -> AppI (resolveVariableNames context t1, resolveVariableNames context t2)
            | LamIV (s, t)   -> LamI (resolveVariableNames (s::context) t)
            | IdentIV s      -> IdentI s

        resolveVariableNames []
