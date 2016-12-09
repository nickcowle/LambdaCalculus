namespace LambdaCalculus

type TreeMap<'k, 'v> =
| Empty
| Node of TreeMap<'k, 'v> * 'k * 'v * TreeMap<'k, 'v>

module TreeMap =

    let rec insert k v =
        function
        | Empty -> Node (Empty, k, v, Empty)
        | Node (l, k', v', r) ->
            if k < k' then
                Node (insert k v l, k', v', r)
            else if k = k' then
                Node (l, k', v, r)
            else
                Node (l, k', v', insert k v r)

    let rec map f =
        function
        | Empty -> Empty
        | Node (l, k, v, r) ->
            let k, v = f k v
            Node (map f l, k, v, map f r)

/// Map implemented as an ordered, unbalanced binary tree
module Map =

    /// emptyT : Map<'k, 'v>
    let emptyT = lams 2 (Var 1)

    /// nodeT : Map<'k, 'v> -> 'k -> 'v -> Map<'k, 'v> -> Map<'k, 'v>
    let nodeT = lams 6 (Var 0 $ Var 5 $ Var 4 $ Var 3 $ Var 2)

    /// insertT : ('k -> 'k -> Ordering) -> 'k -> 'v -> Map<'k, 'v> -> Map<'k, 'v>
    let insertT =
        let onEmpty = nodeT $ emptyT $ Var 2 $ Var 1 $ emptyT
        let onNode =
            let insertLeft  = nodeT $ appVars [ 8 ; 7 ; 6 ; 5 ; 3 ] $ Var 2 $ Var 1 $ Var 0
            let replace     = nodeT $ Var 3 $ Var 2 $ Var 5 $ Var 0
            let insertRight = nodeT $ Var 3 $ Var 2 $ Var 1 $ appVars [ 8 ; 7 ; 6 ; 5 ; 0 ]
            lams 4 (Var 7 $ Var 6 $ Var 2 $ insertLeft $ replace $ insertRight)
        Combinators.Y $ (lams 5 (Var 0 $ onEmpty $ onNode))

    /// findT : ('k -> 'k -> Ordering) -> 'k -> Map<'k, 'v> -> 'v option
    let findT =
        let onNode = lams 4 (Var 6 $ Var 5 $ Var 2 $ appVars [ 7 ; 6 ; 5 ; 3 ] $ (Option.someT $ Var 1) $ appVars [ 7 ; 6 ; 5 ; 0 ])
        Combinators.Y $ (lams 4 (Var 0 $ Option.noneT $ onNode))

    let rec toTerm =
        function
        | Empty -> emptyT
        | Node (l, k, v, r) ->
            lams 2 (Var 0 $ toTerm l $ k $ v $ toTerm r)

    let rec fromTerm =
        function
        | t when t = emptyT -> Some TreeMap.Empty
        | Lam (Lam (App (App (App (App (Var 0, l), k), v), r))) ->
            match fromTerm l, fromTerm r with
            | Some l, Some r ->
                Node (l, k, v, r) |> Some
            | _ -> None
        | _ -> None
