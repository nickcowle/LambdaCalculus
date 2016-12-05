namespace LambdaCalculus

module Combinators =

    /// composeT : ('a -> 'b) -> ('b -> 'c) -> ('a -> 'c)
    let composeT = lams 3 (Var 1 $ (Var 2 $ Var 0))

    /// compose2T : ('a -> 'b -> 'c) -> ('c -> 'd) -> ('a -> 'b -> 'd)
    let compose2T = lams 4 (Var 2 $ (Var 3 $ Var 1 $ Var 0))

    /// The Y combinator
    let Y =
        let t = Lam (Var 1 $ (Var 0 $ Var 0))
        Lam (t $ t)

    /// bindToApplyT : ('a 'm -> ('a -> 'b 'm) -> 'b 'm) -> ('c -> 'c 'm) -> 'a 'm -> ('a -> 'b) 'm -> 'b 'm
    let bindToApplyT = lams 4 (Var 3 $ Var 0 $ Lam (Var 4 $ Var 2 $ (composeT $ Var 0 $ Var 3)))

    /// applyToMapT : ('a 'm -> ('a -> 'b) 'm -> 'b 'm) -> ('c -> 'c 'm) -> 'a 'm -> ('a -> 'b) -> 'b 'm
    let applyToMapT = lams 4 (Var 3 $ Var 1 $ (Var 2 $ Var 0))
