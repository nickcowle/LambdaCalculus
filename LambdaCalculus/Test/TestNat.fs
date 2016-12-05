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
        let one = apps [ Nat.succT ; Nat.zeroT ] |> Eval.eval |> Nat.fromTerm |> Option.get
        Assert.AreEqual(1, one)

    [<Test>]
    member __.``8 + 13 = 21`` () =
        let result = apps [ Nat.addT ; Nat.toTerm 8 ; Nat.toTerm 13 ] |> Eval.eval |> Nat.fromTerm |> Option.get
        Assert.AreEqual(21, result)

    [<Test>]
    member __.``13 + 8 = 21`` () =
        let result = apps [ Nat.addT ; Nat.toTerm 13 ; Nat.toTerm 8 ] |> Eval.eval |> Nat.fromTerm |> Option.get
        Assert.AreEqual(21, result)

    [<Test>]
    member __.``3 * 4 = 12`` () =
        let result = apps [ Nat.multT ; Nat.toTerm 3 ; Nat.toTerm 4 ] |> Eval.eval |> Nat.fromTerm |> Option.get
        Assert.AreEqual(12, result)

    [<Test>]
    member __.``zero is zero`` () =
        let result = (Nat.isZeroT $ Nat.toTerm 0) |> Eval.eval
        Assert.AreEqual(Bool.trueT, result)

    [<Test>]
    member __.``three is not zero`` () =
        let result = (Nat.isZeroT $ Nat.toTerm 3) |> Eval.eval
        Assert.AreEqual(Bool.falseT, result)
