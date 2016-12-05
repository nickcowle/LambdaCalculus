namespace LambdaCalculus.Test

open LambdaCalculus
open NUnit.Framework

[<TestFixture>]
type TestBool () =

    [<Test>]
    member __.``true <> false`` () =
        Assert.AreNotEqual(Bool.trueT, Bool.falseT)

    [<Test>]
    member __.``and laws hold`` () =
        let combinations =
            [
                Bool.falseT, Bool.falseT, Bool.falseT
                Bool.falseT, Bool.trueT,  Bool.falseT
                Bool.trueT,  Bool.falseT, Bool.falseT
                Bool.trueT,  Bool.trueT,  Bool.trueT
            ]

        let testCombination (b1, b2, expected) =
            let actual = (Bool.andT $ b1 $ b2) |> Eval.eval
            Assert.AreEqual(expected, actual)

        combinations |> Seq.iter testCombination

    [<Test>]
    member __.``or laws hold`` () =
        let combinations =
            [
                Bool.falseT, Bool.falseT, Bool.falseT
                Bool.falseT, Bool.trueT,  Bool.trueT
                Bool.trueT,  Bool.falseT, Bool.trueT
                Bool.trueT,  Bool.trueT,  Bool.trueT
            ]

        let testCombination (b1, b2, expected) =
            let actual = (Bool.orT $ b1 $ b2) |> Eval.eval
            Assert.AreEqual(expected, actual)

        combinations |> Seq.iter testCombination
    [<Test>]
    member __.``not laws hold`` () =

        let notTrue = (Bool.notT $ Bool.trueT) |> Eval.eval
        Assert.AreEqual(Bool.falseT, notTrue)

        let notFalse = (Bool.notT $ Bool.falseT) |> Eval.eval
        Assert.AreEqual(Bool.trueT, notFalse)
