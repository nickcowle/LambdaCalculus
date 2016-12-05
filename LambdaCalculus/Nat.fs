namespace LambdaCalculus

module Nat =

    /// zeroT : Nat
    let zeroT = lams 2 (Var 0)

    /// succT : Nat -> Nat
    let succT = lams 3 (Var 1 $ appVars [ 2 ; 1 ; 0])

    /// addT : Nat -> Nat -> Nat
    let addT = lams 4 (Var 2 $ Var 1 $ appVars [ 3 ; 1 ; 0 ])

    /// multT : Nat -> Nat -> Nat
    let multT = lams 4 (Var 2 $ appVars [ 3 ; 1 ] $ Var 0)

    /// isZeroT : Nat -> bool
    let isZeroT = Lam (Var 0 $ Lam (Bool.falseT) $ Bool.trueT)

    let toTerm i =
        let rec body = function 0 -> Var 0 | n -> (Var 1 $ body (n-1))
        lams 2 (body i)

    let fromTerm =
        let rec count =
            function
            | Var 0 -> Some 0
            | App (Var 1, t') -> (count t') |> Option.map ((+) 1)
            | _ -> None

        function Lam (Lam t') -> count t' | _ -> None
