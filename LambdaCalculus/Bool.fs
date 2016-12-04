namespace LambdaCalculus

module Bool =

    /// trueT : bool
    let trueT  = lams 2 (Var 1)

    /// falseT : bool
    let falseT = lams 2 (Var 0)

    /// andT : bool -> bool -> bool
    let andT = lams 4 (apps [ Var 3 ; appVars [ 2 ; 1 ; 0] ; Var 0 ])

    /// orT : bool -> bool -> bool
    let orT = lams 4 (apps [ Var 3 ; Var 1 ; appVars [ 2 ; 1 ; 0] ])

    /// notT : bool -> bool
    let notT = lams 3 (appVars [ 2 ; 0 ; 1 ])

    /// ifT : bool -> 'a -> 'a -> 'a
    let ifT = lams 3 (appVars [ 2 ; 1 ; 0 ])

    let toTerm =
        function
        | true  -> trueT
        | false -> falseT

    let fromTerm =
        function
        | t when t = trueT  -> Some true
        | t when t = falseT -> Some false
        | _                 -> None
