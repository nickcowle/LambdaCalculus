namespace LambdaCalculus

module Eval =

    let reindexFreeVariables (increaseBy : int) (s : Term) =
        let rec reindex (bound : int) =
            function
            | Var i -> Var (if i >= bound then i + increaseBy else i)
            | App (t1, t2) -> App (reindex bound t1, reindex bound t2)
            | Lam t -> Lam (reindex (bound + 1) t)
        reindex 0 s

    let rec substitute (s : Term) (i : int) (t : Term) =
        match t with
        | Var i' when i' = i -> reindexFreeVariables i s
        | Var i' when i' > i -> Var (i' - 1)
        | Var i'             -> Var i'
        | App (t1, t2) -> App (substitute s i t1, substitute s i t2)
        | Lam t -> Lam (substitute s (i+1) t)

    let rec evald t =
        match t with
        | Var i -> Var i
        | Lam t -> Lam (evald t)
        | App (t1, t2) ->
            let t1 = evald t1
            let t2 = evald t2
            match t1 with
            | Lam t1' -> substitute t2 0 t1' |> evald
            | _ -> App (t1, t2)

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

    let reindexFreeVariablesl (increaseBy : int) (s : LazyTerm Lazy) =
        let rec reindex (bound : int) (t : LazyTerm Lazy) =
            lazy
                match t.Value with
                | LVar i -> LVar (if i >= bound then i + increaseBy else i)
                | LApp (t1, t2) -> LApp (reindex bound t1, reindex bound t2)
                | LLam t -> LLam (reindex (bound + 1) t)
        reindex 0 s

    let rec substitutel (s : LazyTerm Lazy) (i : int) (t : LazyTerm Lazy) =
        lazy
            match t.Value with
            | LVar i' when i' = i -> (reindexFreeVariablesl i s) |> (fun l -> l.Value)
            | LVar i' when i' > i -> LVar (i' - 1)
            | LVar i'             -> LVar i'
            | LApp (t1, t2)       -> LApp (substitutel s i t1, substitutel s i t2)
            | LLam t              -> LLam (substitutel s (i+1) t)

    let rec evall (t : LazyTerm Lazy) =
        lazy
            match t.Value with
            | LVar i -> LVar i
            | LLam t -> LLam (evall t)
            | LApp (t1, t2) ->
                let t1 = evall t1
                let t2 = evall t2
                match t1.Value with
                | LLam t1' -> (substitutel t2 0 t1') |> evall |> (fun l -> l.Value)
                | _ -> LApp (t1, t2)

    let eval = toLazy >> evall >> fromLazy
