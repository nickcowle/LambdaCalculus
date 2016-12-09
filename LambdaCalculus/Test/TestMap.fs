namespace LambdaCalculus.Test

open LambdaCalculus
open NUnit.Framework

[<TestFixture>]
type TestMap () =

    let testMap =
        let singleton k v = Node (Empty, k, v, Empty)
        let left = Node (singleton 1 4, 3, 1, Empty)
        let right = Node (singleton 5 1, 6, 2, singleton 8 5)
        Node (left, 4, 7, right)

    let mapToTerms   = TreeMap.map (fun k v -> Nat.toTerm k, Nat.toTerm v)
    let mapFromTerms = TreeMap.map (fun k v -> Nat.fromTerm k |> Option.get, Nat.fromTerm v |> Option.get)

    let testMapT = testMap |> mapToTerms |> Map.toTerm

    [<Test>]
    member __.``toTerm and fromTerm round trips`` () =

        let result =
            testMap |> mapToTerms |> Map.toTerm |> Map.fromTerm |> Option.get |> mapFromTerms

        Assert.AreEqual(testMap, result)

    [<Test>]
    member __.``fromTerm and toTerm round trips`` () =

        let result =
            testMapT |> Map.fromTerm |> Option.get |> mapFromTerms |> mapToTerms |> Map.toTerm

        Assert.AreEqual(testMapT, result)

    [<Test>]
    member __.``test insert`` () =

        let expected = TreeMap.insert 2 3 testMap

        let result =
            (Map.insertT $ Nat.compareT $ Nat.toTerm 2 $ Nat.toTerm 3 $ testMapT) |> Eval.eval
            |> Map.fromTerm |> Option.get |> mapFromTerms

        Assert.AreEqual(expected, result)

    [<Test>]
    member __.``test find`` () =

        let tests =
            [
                1, Some 4
                2, None
                3, Some 1
                4, Some 7
                5, Some 1
                6, Some 2
                7, None
                8, Some 5
            ]

        for (k, expected) in tests do
            let result = (Map.findT $ Nat.compareT $ Nat.toTerm k $ testMapT) |> Eval.eval
            let expected = expected |> Option.map Nat.toTerm |> Option.toTerm
            Assert.AreEqual(expected, result)
