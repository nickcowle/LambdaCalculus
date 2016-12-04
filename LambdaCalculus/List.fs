﻿namespace LambdaCalculus

module List =

    /// emptyT : 'a list
    let emptyT = lams 2 (Var 1)

    /// consT : 'a -> 'a list -> 'a list
    let consT = lams 4 (appVars [ 0 ; 3 ; 2 ])

    /// isEmptyT : 'a list -> bool
    let isEmptyT = lams 3 (apps [ Var 2 ; Var 1 ; lams 2 (Var 2) ])

    /// repeatT : Nat -> 'a -> 'a list
    let repeatT = lams 2 (apps [ Var 1 ; App (consT, Var 0) ; emptyT ])

    /// foldT : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
    let foldT = App (Functions.Y, lams 4 (apps [ Var 0 ; Var 1 ; lams 2 (apps [ Var 4 ; appVars [ 5 ; 4 ; 3 ; 0 ] ; Var 1 ]) ]))

    /// lengthT : 'a list -> Nat
    let lengthT = apps [ foldT ; lams 2 (Nat.succT $ Var 1) ; Nat.zeroT ]

    /// appendT : 'a list -> 'a list -> 'a list
    let appendT = App (Functions.Y, lams 3 (apps [ Var 1 ; Var 0 ; lams 2 (apps [ consT ; Var 1 ; appVars [ 4 ; 0 ; 2 ] ]) ]))

    /// collectT : ('a -> 'b list) -> ' a list -> 'b list
    let collectT = lams 2 (apps [ foldT ; lams 2 (apps [ appendT ; Var 3 $ Var 0 ; Var 1 ]) ; emptyT ; Var 0 ])

    /// bindT : ' a list -> ('a -> 'b list) -> 'b list
    let bindT = lams 2 (collectT $ Var 0 $ Var 1)

    /// returnT : 'a -> 'a list
    let returnT = Lam (consT $ Var 0 $ emptyT)

    /// applyT : 'a list -> ('a -> 'b) list -> 'b list
    let applyT = Functions.bindToApplyT $ bindT $ returnT

    /// mapT : 'a list -> ('a -> 'b) -> 'b list
    let mapT = Functions.applyToMapT $ applyT $ returnT

    // Can this be eval'd for a more optimal function? Yes.
    /// tryHeadT : 'a list -> 'a option
    let tryHeadT = Lam (apps [ Var 0 ; Option.noneT ; lams 2 (Option.someT $ Var 1) ])

    /// sumT : Nat list -> Nat
    let sumT = apps [ foldT ; Nat.addT ; Nat.zeroT ]

    /// productT : Nat list -> Nat
    let productT = apps [ foldT ; Nat.multT ; Nat.succT $ Nat.zeroT ]

    let rec toTerm =
        function
        | [] -> emptyT
        | t::ts -> lams 2 (apps [ Var 0 ; t ; toTerm ts ])

    let rec fromTerm =
        function
        | t when t = emptyT -> Some []
        | Lam (Lam (App (App (Var 0, h), t))) ->
            fromTerm t |> Option.map (fun t -> h::t)
        | _ -> None