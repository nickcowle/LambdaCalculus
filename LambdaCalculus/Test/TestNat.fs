namespace LambdaCalculus.Test

open LambdaCalculus
open NUnit.Framework

[<TestFixture>]
type TestNat () =

    [<Test>]
    member __.``zero round trips`` () =
        let roundTripped = 0 |> Nat.toTerm |> Nat.fromTerm |> Option.get
        Assert.AreEqual(0, roundTripped)

    [<Test>]
    member __.``zeroT round trips`` () =
        let roundTripped = Nat.zeroT |> Nat.fromTerm |> Option.get |> Nat.toTerm
        Assert.AreEqual(Nat.zeroT, roundTripped)

    [<Test>]
    member __.``succ zero = one`` () =
        let one = (Nat.succT $ Nat.zeroT) |> Eval.eval |> Nat.fromTerm |> Option.get
        Assert.AreEqual(1, one)

    [<Test>]
    member __.``8 + 13 = 21`` () =
        let result = (Nat.addT $ Nat.toTerm 8 $ Nat.toTerm 13) |> Eval.eval |> Nat.fromTerm |> Option.get
        Assert.AreEqual(21, result)

    [<Test>]
    member __.``13 + 8 = 21`` () =
        let result = (Nat.addT $ Nat.toTerm 13 $ Nat.toTerm 8) |> Eval.eval |> Nat.fromTerm |> Option.get
        Assert.AreEqual(21, result)

    [<Test>]
    member __.``3 * 4 = 12`` () =
        let result = (Nat.multT $ Nat.toTerm 3 $ Nat.toTerm 4) |> Eval.eval |> Nat.fromTerm |> Option.get
        Assert.AreEqual(12, result)

    [<Test>]
    member __.``pred zero = zero`` () =
        let result = (Nat.predT $ Nat.zeroT) |> Eval.eval
        Assert.AreEqual(Nat.zeroT, result)

    [<Test>]
    member __.``pred one = zero`` () =
        let result = (Nat.predT $ Nat.toTerm 1) |> Eval.eval
        Assert.AreEqual(Nat.zeroT, result)

    [<Test>]
    member __.``pred two = one`` () =
        let result = (Nat.predT $ Nat.toTerm 2) |> Eval.eval
        Assert.AreEqual(Nat.toTerm 1, result)

    [<Test>]
    member __.``21 - 3 = 18`` () =
        let result = (Nat.subtractT $ Nat.toTerm 21 $ Nat.toTerm 3) |> Eval.eval
        Assert.AreEqual(Nat.toTerm 18, result)

    [<Test>]
    member __.``zero is zero`` () =
        let result = (Nat.isZeroT $ Nat.toTerm 0) |> Eval.eval
        Assert.AreEqual(Bool.trueT, result)

    [<Test>]
    member __.``three is not zero`` () =
        let result = (Nat.isZeroT $ Nat.toTerm 3) |> Eval.eval
        Assert.AreEqual(Bool.falseT, result)

    [<Test>]
    member __.``5 eq 5`` () =
        let result = (Nat.eqT $ Nat.toTerm 5 $ Nat.toTerm 5) |> Eval.eval
        Assert.AreEqual(Bool.trueT, result)

    [<Test>]
    member __.``5 not eq 6`` () =
        let result = (Nat.eqT $ Nat.toTerm 5 $ Nat.toTerm 6) |> Eval.eval
        Assert.AreEqual(Bool.falseT, result)

    [<Test>]
    member __.``5 not eq 4`` () =
        let result = (Nat.eqT $ Nat.toTerm 5 $ Nat.toTerm 4) |> Eval.eval
        Assert.AreEqual(Bool.falseT, result)
