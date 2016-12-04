namespace LambdaCalculus.Test

open LambdaCalculus
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestBool () =

    [<TestMethod>]
    member __.``true <> false`` () =
        Assert.AreNotEqual(Bool.trueT, Bool.falseT)

    [<TestMethod>]
    member __.``and laws hold`` () =
        let combinations =
            [
                Bool.falseT, Bool.falseT, Bool.falseT
                Bool.falseT, Bool.trueT,  Bool.falseT
                Bool.trueT,  Bool.falseT, Bool.falseT
                Bool.trueT,  Bool.trueT,  Bool.trueT
            ]

        let testCombination (b1, b2, expected) =
            let actual = apps [ Bool.andT ; b1 ; b2 ] |> Eval.eval
            Assert.AreEqual<_>(expected, actual)

        combinations |> Seq.iter testCombination

    [<TestMethod>]
    member __.``or laws hold`` () =
        let combinations =
            [
                Bool.falseT, Bool.falseT, Bool.falseT
                Bool.falseT, Bool.trueT,  Bool.trueT
                Bool.trueT,  Bool.falseT, Bool.trueT
                Bool.trueT,  Bool.trueT,  Bool.trueT
            ]

        let testCombination (b1, b2, expected) =
            let actual = apps [ Bool.orT ; b1 ; b2 ] |> Eval.eval
            Assert.AreEqual<_>(expected, actual)

        combinations |> Seq.iter testCombination
    [<TestMethod>]
    member __.``not laws hold`` () =

        let notTrue = App(Bool.notT, Bool.trueT) |> Eval.eval
        Assert.AreEqual<_>(Bool.falseT, notTrue)

        let notFalse = App(Bool.notT, Bool.falseT) |> Eval.eval
        Assert.AreEqual<_>(Bool.trueT, notFalse)
