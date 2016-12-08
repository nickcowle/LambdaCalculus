namespace LambdaCalculus.Test

open LambdaCalculus
open NUnit.Framework

[<TestFixture>]
type TestPair () =

    [<Test>]
    member __.``both ways of constructing a pair are equivalent`` () =
        let five = Nat.toTerm 5
        let eight = Nat.toTerm 8
        let p1 = Pair.toTerm (five, eight)
        let p2 = (Pair.makeT $ five $ eight) |> Eval.eval
        Assert.AreEqual(p1, p2)

    [<Test>]
    member __.``both ways of getting first are equivalent`` () =
        let pair = Pair.toTerm (Nat.toTerm 5, Nat.toTerm 8)
        let f1 = pair |> Pair.fromTerm |> Option.get |> fst
        let f2 = (Pair.firstT $ pair) |> Eval.eval
        Assert.AreEqual(f1, f2)

    [<Test>]
    member __.``both ways of getting second are equivalent`` () =
        let pair = Pair.toTerm (Nat.toTerm 5, Nat.toTerm 8)
        let s1 = pair |> Pair.fromTerm |> Option.get |> snd
        let s2 = (Pair.secondT $ pair) |> Eval.eval
        Assert.AreEqual(s1, s2)

    [<Test>]
    member __.``pair creation and project round trips`` () =

        let f = Nat.toTerm 5
        let s = Nat.toTerm 8
        let pair = (Pair.makeT $ f $ s ) |> Eval.eval
        let f'   = (Pair.firstT  $ pair) |> Eval.eval
        let s'   = (Pair.secondT $ pair) |> Eval.eval

        Assert.AreEqual(f, f')
        Assert.AreEqual(s, s')

    [<Test>]
    member __.``pair projection and creation round trips`` () =
        let pair  = Pair.toTerm (Nat.toTerm 5, Nat.toTerm 8)
        let f     = (Pair.firstT  $ pair) |> Eval.eval
        let s     = (Pair.secondT $ pair) |> Eval.eval
        let pair' = (Pair.makeT $ f $ s ) |> Eval.eval

        Assert.AreEqual(pair, pair')

    [<Test>]
    member __.``flip is self-inverse`` () =
        let pair  = Pair.toTerm (Nat.toTerm 5, Nat.toTerm 8)
        let pair' = Pair.flipT $ (Pair.flipT $ pair) |> Eval.eval
        Assert.AreEqual (pair, pair')

    [<Test>]
    member __.``(pair >> flip) >> flip evals to pair`` () =
        let result = (Combinators.compose2T $ (Combinators.compose2T $ Pair.makeT $ Pair.flipT) $ Pair.flipT) |> Eval.eval
        Assert.AreEqual(Pair.makeT, result)

    [<Test>]
    member __.``pair >> (flip >> flip) evals to pair`` () =
        let result = (Combinators.compose2T $ Pair.makeT $ (Combinators.composeT $ Pair.flipT $ Pair.flipT)) |> Eval.eval
        Assert.AreEqual(Pair.makeT, result)

    // any other composition optimisations
