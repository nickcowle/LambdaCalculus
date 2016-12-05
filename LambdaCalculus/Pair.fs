namespace LambdaCalculus

module Pair =

    /// pairT : 'a -> 'b -> ('a * 'b)
    let pairT = lams 3 (appVars [ 0 ; 2 ; 1])

    /// firstT : ('a * 'b) -> 'a
    let firstT = Lam (Var 0 $ lams 2 (Var 1))

    /// secondT : ('a * 'b) -> 'b
    let secondT = Lam (Var 0 $ lams 2 (Var 0))

    /// flipT : ('a * 'b) -> ('b * 'a)
    let flipT = Lam (Var 0 $ lams 3 (appVars [ 0 ; 1 ; 2 ]))

    let toTerm (t1, t2) = Lam (Var 0 $ t1 $ t2)

    let fromTerm t =
        match t with
        | Lam (App (App (Var 0, t1), t2)) -> Some (t1, t2)
        | _ -> None
