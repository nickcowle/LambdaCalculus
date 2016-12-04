namespace LambdaCalculus.Test

open LambdaCalculus
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestNat () =

    [<TestMethod>]
    member __.``zero round trips`` () =
        let roundTripped = 0 |> Nat.toTerm |> Nat.fromTerm |> Option.get
        Assert.AreEqual<_>(0, roundTripped)

    [<TestMethod>]
    member __.``zeroT round trips`` () =
        let roundTripped = Nat.zeroT |> Nat.fromTerm |> Option.get |> Nat.toTerm
        Assert.AreEqual<_>(Nat.zeroT, roundTripped)

    [<TestMethod>]
    member __.``succ zero = one`` () =
        let one = apps [ Nat.succT ; Nat.zeroT ] |> Eval.eval |> Nat.fromTerm |> Option.get
        Assert.AreEqual<_>(1, one)

    [<TestMethod>]
    member __.``8 + 13 = 21`` () =
        let result = apps [ Nat.addT ; Nat.toTerm 8 ; Nat.toTerm 13 ] |> Eval.eval |> Nat.fromTerm |> Option.get
        Assert.AreEqual<_>(21, result)

    [<TestMethod>]
    member __.``13 + 8 = 21`` () =
        let result = apps [ Nat.addT ; Nat.toTerm 13 ; Nat.toTerm 8 ] |> Eval.eval |> Nat.fromTerm |> Option.get
        Assert.AreEqual<_>(21, result)

    [<TestMethod>]
    member __.``3 * 4 = 12`` () =
        let result = apps [ Nat.multT ; Nat.toTerm 3 ; Nat.toTerm 4 ] |> Eval.eval |> Nat.fromTerm |> Option.get
        Assert.AreEqual<_>(12, result)

    [<TestMethod>]
    member __.``zero is zero`` () =
        let result = (Nat.isZeroT $ Nat.toTerm 0) |> Eval.eval
        Assert.AreEqual<_>(Bool.trueT, result)

    [<TestMethod>]
    member __.``three is not zero`` () =
        let result = (Nat.isZeroT $ Nat.toTerm 3) |> Eval.eval
        Assert.AreEqual<_>(Bool.falseT, result)
