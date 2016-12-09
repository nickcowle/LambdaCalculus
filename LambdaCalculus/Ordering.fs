namespace LambdaCalculus

type Ordering =
| LessThan
| Equal
| GreaterThan

/// The result of a comparison operation on two comparable elements
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Ordering =

    /// lessThanT : Ordering
    let lessThanT = lams 3 (Var 2)

    /// equalT : Ordering
    let equalT = lams 3 (Var 1)

    /// greaterThanT : Ordering
    let greaterThanT = lams 3 (Var 0)

    let toTerm =
        function
        | LessThan    -> lessThanT
        | Equal       -> equalT
        | GreaterThan -> greaterThanT

    let fromTerm =
        function
        | t when t = lessThanT    -> Some LessThan
        | t when t = equalT       -> Some Equal
        | t when t = greaterThanT -> Some GreaterThan
        | _ -> None
