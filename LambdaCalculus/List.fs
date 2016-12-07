namespace LambdaCalculus

module List =

    /// emptyT : 'a list
    let emptyT = lams 2 (Var 1)

    /// consT : 'a -> 'a list -> 'a list
    let consT = lams 4 (appVars [ 0 ; 3 ; 2 ])

    /// isEmptyT : 'a list -> bool
    let isEmptyT = lams 3 (Var 2 $ Var 1 $ lams 2 (Var 2))

    /// repeatT : Nat -> 'a -> 'a list
    let repeatT = lams 2 (Var 1 $ (consT $ Var 0) $ emptyT)

    /// foldT : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
    let foldT = Combinators.Y $ lams 4 (Var 0 $ Var 1 $ lams 2 (Var 4 $ appVars [ 5 ; 4 ; 3 ; 0 ] $ Var 1))

    /// lengthT : 'a list -> Nat
    let lengthT = foldT $ lams 2 (Nat.succT $ Var 1) $ Nat.zeroT

    /// appendT : 'a list -> 'a list -> 'a list
    let appendT = Combinators.Y $ lams 3 (Var 1 $ Var 0 $ lams 2 (consT $ Var 1 $ appVars [ 4 ; 0 ; 2 ]))

    /// collectT : ('a -> 'b list) -> ' a list -> 'b list
    let collectT = lams 2 (foldT $ lams 2 (appendT $ (Var 3 $ Var 0) $ Var 1) $ emptyT $ Var 0)

    /// bindT : ' a list -> ('a -> 'b list) -> 'b list
    let bindT = lams 2 (collectT $ Var 0 $ Var 1)

    /// returnT : 'a -> 'a list
    let returnT = Lam (consT $ Var 0 $ emptyT)

    /// applyT : 'a list -> ('a -> 'b) list -> 'b list
    let applyT = Combinators.bindToApplyT $ bindT $ returnT

    /// mapT : 'a list -> ('a -> 'b) -> 'b list
    let mapT = Combinators.applyToMapT $ applyT $ returnT

    // Can this be eval'd for a more optimal function? Yes.
    /// tryHeadT : 'a list -> 'a option
    let tryHeadT = Lam (Var 0 $ Option.noneT $ lams 2 (Option.someT $ Var 1))

    /// sumT : Nat list -> Nat
    let sumT = foldT $ Nat.addT $ Nat.zeroT

    /// productT : Nat list -> Nat
    let productT = foldT $ Nat.multT $ (Nat.succT $ Nat.zeroT)

    /// takeT : Nat -> 'a list -> 'a list
    let takeT = Combinators.Y $ lams 3 ((Nat.isZeroT $ Var 1) $ emptyT $ (Var 0 $ emptyT $ lams 2 (consT $ Var 1 $ (Var 4 $ (Nat.predT $ Var 3) $ Var 0))))

    /// skipT : Nat -> 'a list -> 'a list
    let skipT = lams 2 (Var 1 $ Lam (Var 0 $ emptyT $ lams 2 (Var 0)) $ Var 0)

    /// natsT : Nat list
    let natsT = Combinators.Y $ lams 2 (consT $ Var 0 $ (Var 1 $ (Nat.succT $ Var 0))) $ Nat.zeroT

    let rec toTerm =
        function
        | [] -> emptyT
        | t::ts -> lams 2 (Var 0 $ t $ toTerm ts)

    let rec fromTerm =
        function
        | t when t = emptyT -> Some []
        | Lam (Lam (App (App (Var 0, h), t))) ->
            fromTerm t |> Option.map (fun t -> h::t)
        | _ -> None
