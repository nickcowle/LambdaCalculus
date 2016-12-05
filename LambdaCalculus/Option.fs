namespace LambdaCalculus

module Option =

    /// noneT : 'a option
    let noneT = lams 2 (Var 1)

    /// someT : 'a -> 'a option
    let someT = lams 3 (appVars [ 0 ; 2 ])

    // What about writing this in the naive way and evaling it?
    /// bindT : 'a option -> ('a -> 'b option) -> 'b option
    let bindT = lams 2 (Var 1 $ (lams 2 (Var 1)) $ Var 0 )

    /// returnT : 'a -> 'a option
    let returnT = someT

    /// applyT : 'a option -> ('a -> 'b) option -> 'b option
    let applyT = Functions.bindToApplyT $ bindT $ returnT

    /// mapT : 'a option -> ('a -> 'b) -> 'b option
    let mapT = Functions.applyToMapT $ applyT $ returnT

    // Can this be eval'd?
    /// isNoneT : 'a option -> bool
    let isNoneT = Lam (Var 0 $ Bool.trueT $ Lam Bool.falseT)

    // Can this be eval'd?
    /// isSomeT : 'a option -> bool
    let isSomeT = Lam (Var 0 $ Bool.falseT $ Lam Bool.trueT)

    let toTerm =
        function
        | None   -> noneT
        | Some t -> lams 2 (Var 0 $ t )

    let fromTerm =
        function
        | t when t = noneT -> Some None
        | Lam (Lam (App (Var 0, t))) -> Some (Some t)
        | _ -> None
