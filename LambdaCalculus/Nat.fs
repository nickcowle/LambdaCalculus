﻿namespace LambdaCalculus

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

    /// predT : Nat -> Nat
    let predT =
        let s = Lam (Pair.makeT $ Bool.trueT $ (Pair.firstT $ Var 0 $ (Var 2 $ (Pair.secondT $ Var 0)) $ (Pair.secondT $ Var 0)))
        let z = Pair.makeT $ Bool.falseT $ Var 0
        lams 3 (Pair.secondT $ (Var 2 $ s $ z))

    /// subtractT : Nat -> Nat -> Nat
    let subtractT = lams 2 (Var 0 $ predT $ Var 1)

    /// equalsT : Nat -> Nat -> bool
    let equalsT =
        let s = Pair.makeT $ Bool.falseT
        let z = Pair.makeT $ Bool.trueT $ (Combinators.Y $ Lam (Pair.makeT $ Bool.falseT $ Var 0))
        lams 2 (Pair.firstT $ (Var 1 $ Pair.secondT $ (Var 0 $ s $ z)))

    /// compareT : Nat -> Nat -> Ordering
    let compareT =
        let s = Pair.makeT $ Ordering.lessThanT
        let z = Pair.makeT $ Ordering.equalT $ (Combinators.Y $ Lam (Pair.makeT $ Ordering.greaterThanT $ Var 0))
        lams 2 (Pair.firstT $ (Var 1 $ Pair.secondT $ (Var 0 $ s $ z)))

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
