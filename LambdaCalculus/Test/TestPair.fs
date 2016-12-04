namespace LambdaCalculus.Test

open LambdaCalculus
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestPair () =

    [<TestMethod>]
    member __.``both ways of constructing a pair are equivalent`` () =
        let five = Nat.toTerm 5
        let eight = Nat.toTerm 8
        let p1 = Pair.toTerm (five, eight)
        let p2 = apps [ Pair.pairT ; five ; eight ] |> Eval.eval
        Assert.AreEqual<_>(p1, p2)

    [<TestMethod>]
    member __.``both ways of getting first are equivalent`` () =
        let pair = Pair.toTerm (Nat.toTerm 5, Nat.toTerm 8)
        let f1 = pair |> Pair.fromTerm |> Option.get |> fst
        let f2 = apps [ Pair.firstT ; pair ] |> Eval.eval
        Assert.AreEqual<_>(f1, f2)

    [<TestMethod>]
    member __.``both ways of getting second are equivalent`` () =
        let pair = Pair.toTerm (Nat.toTerm 5, Nat.toTerm 8)
        let s1 = pair |> Pair.fromTerm |> Option.get |> snd
        let s2 = apps [ Pair.secondT ; pair ] |> Eval.eval
        Assert.AreEqual<_>(s1, s2)

    [<TestMethod>]
    member __.``pair creation and project round trips`` () =

        let f = Nat.toTerm 5
        let s = Nat.toTerm 8
        let pair = apps [ Pair.pairT ; f ; s ]  |> Eval.eval
        let f'   = apps [ Pair.firstT ; pair ]  |> Eval.eval
        let s'   = apps [ Pair.secondT ; pair ] |> Eval.eval

        Assert.AreEqual<_>(f, f')
        Assert.AreEqual<_>(s, s')

    [<TestMethod>]
    member __.``pair projection and creation round trips`` () =
        let pair  = Pair.toTerm (Nat.toTerm 5, Nat.toTerm 8)
        let f     = apps [ Pair.firstT ; pair ]  |> Eval.eval
        let s     = apps [ Pair.secondT ; pair ] |> Eval.eval
        let pair' = apps [ Pair.pairT ; f ; s ]  |> Eval.eval

        Assert.AreEqual<_>(pair, pair')

    [<TestMethod>]
    member __.``flip is self-inverse`` () =
        let pair  = Pair.toTerm (Nat.toTerm 5, Nat.toTerm 8)
        let pair' = App (Pair.flipT, App (Pair.flipT, pair)) |> Eval.eval
        Assert.AreEqual<_> (pair, pair')

    [<TestMethod>]
    member __.``(pair >> flip) >> flip evals to pair`` () =
        let result = apps [ Functions.compose2T ; apps [ Functions.compose2T ; Pair.pairT ; Pair.flipT ] ; Pair.flipT ] |> Eval.eval
        Assert.AreEqual<_>(Pair.pairT, result)

    [<TestMethod>]
    member __.``pair >> (flip >> flip) evals to pair`` () =
        let result = apps [ Functions.compose2T ; Pair.pairT ; apps [ Functions.composeT ; Pair.flipT ; Pair.flipT ] ] |> Eval.eval
        Assert.AreEqual<_>(Pair.pairT, result)

    // any other composition optimisations
