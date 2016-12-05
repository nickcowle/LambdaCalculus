namespace LambdaCalculus

module Eval =

    type LazyTerm =
    | LVar of int
    | LLam of LazyTerm Lazy
    | LApp of (LazyTerm Lazy * LazyTerm Lazy)

    let rec toLazy t =
        lazy
            match t with
            | Var i -> LVar i
            | Lam t -> LLam (toLazy t)
            | App (t1, t2) -> LApp (toLazy t1, toLazy t2)

    let rec fromLazy (t : LazyTerm Lazy) =
        match t.Value with
        | LVar i -> Var i
        | LLam t -> Lam (fromLazy t)
        | LApp (t1, t2) -> App (fromLazy t1, fromLazy t2)

    let reindexFreeVariables (increaseBy : int) (s : LazyTerm Lazy) =
        let rec reindex (bound : int) (t : LazyTerm Lazy) =
            lazy
                match t.Value with
                | LVar i -> LVar (if i >= bound then i + increaseBy else i)
                | LApp (t1, t2) -> LApp (reindex bound t1, reindex bound t2)
                | LLam t -> LLam (reindex (bound + 1) t)
        reindex 0 s

    let rec substitute (s : LazyTerm Lazy) (i : int) (t : LazyTerm Lazy) =
        lazy
            match t.Value with
            | LVar i' when i' = i -> (reindexFreeVariables i s) |> (fun l -> l.Value)
            | LVar i' when i' > i -> LVar (i' - 1)
            | LVar i'             -> LVar i'
            | LApp (t1, t2)       -> LApp (substitute s i t1, substitute s i t2)
            | LLam t              -> LLam (substitute s (i+1) t)

    let rec evalLazy (t : LazyTerm Lazy) =
        lazy
            match t.Value with
            | LVar i -> LVar i
            | LLam t -> LLam (evalLazy t)
            | LApp (t1, t2) ->
                let t1 = evalLazy t1
                let t2 = evalLazy t2
                match t1.Value with
                | LLam t1' -> (substitute t2 0 t1') |> evalLazy |> (fun l -> l.Value)
                | _ -> LApp (t1, t2)

    let eval = toLazy >> evalLazy >> fromLazy
