namespace LambdaCalculus.Test

open LambdaCalculus
open NUnit.Framework

[<TestFixture>]
type TestOption () =

    [<Test>]
    member __.``none is none`` () =
        let result = (Option.isNoneT $ Option.noneT) |> Eval.eval
        Assert.AreEqual(Bool.trueT, result)

    [<Test>]
    member __.``none is not some`` () =
        let result = (Option.isSomeT $ Option.noneT) |> Eval.eval
        Assert.AreEqual(Bool.falseT, result)

    [<Test>]
    member __.``some is some`` () =
        let v = (Option.someT $ Nat.toTerm 5)
        let result = (Option.isSomeT $ v) |> Eval.eval
        Assert.AreEqual(Bool.trueT, result)

    [<Test>]
    member __.``some is not none`` () =
        let v = (Option.someT $ Nat.toTerm 5)
        let result = (Option.isNoneT $ v) |> Eval.eval
        Assert.AreEqual(Bool.falseT, result)

    [<Test>]
    member __.``both ways of constructing some are equivalent`` () =
        let s1 = (Option.someT $ Nat.toTerm 5) |> Eval.eval
        let s2 = Option.toTerm (5 |> Nat.toTerm |> Some)
        Assert.AreEqual(s1, s2)

    [<Test>]
    member __.``toTerm and fromTerm round trip`` () =
        Assert.AreEqual(None, None |> Option.toTerm |> Option.fromTerm |> Option.get)
        Assert.AreEqual(Some 8, 8 |> Nat.toTerm |> Some |> Option.toTerm |> Option.fromTerm |> Option.get |> Option.map (Nat.fromTerm >> Option.get))

    [<Test>]
    member __.``fromTerm and toTerm round trip`` () =
        Assert.AreEqual(Option.noneT, Option.noneT |> Option.fromTerm |> Option.get |> Option.toTerm)
        let v = (Option.someT $ Nat.toTerm 8) |> Eval.eval
        Assert.AreEqual(v, v |> Option.fromTerm |> Option.get |> Option.toTerm)

    [<Test>]
    member __.``test applyT`` () =
        let v = Nat.toTerm 12
        let f = Nat.succT
        let none  = Option.noneT
        let someV = Option.someT $ v
        let someF = Option.someT $ f
        let some13 = (Option.someT $ Nat.toTerm 13) |> Eval.eval

        let combinations =
            [
                none,  none,  none
                none,  someF, none
                someV, none,  none
                someV, someF, some13
            ]

        let test (v, f, expected) = Assert.AreEqual(expected, apps [ Option.applyT ; v ; f ] |> Eval.eval)
        combinations |> List.iter test

    [<Test>]
    member __.``test mapT`` () =

        let v = Nat.toTerm 12
        let f = Nat.succT
        let none  = Option.noneT
        let someV = Option.someT $ v
        let some13 = (Option.someT $ Nat.toTerm 13) |> Eval.eval

        let combinations =
            [
                none,  none
                someV, some13
            ]

        let test (v, expected) = Assert.AreEqual(expected, apps [ Option.mapT ; v ; f ] |> Eval.eval)
        combinations |> List.iter test
