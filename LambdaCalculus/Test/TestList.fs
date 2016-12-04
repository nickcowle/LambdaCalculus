namespace LambdaCalculus.Test

open LambdaCalculus
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestList () =

    [<TestMethod>]
    member __.``toTerm and fromTerm round trips`` () =
        let list = [1..5] |> List.rev
        let result = list |> List.map Nat.toTerm |> List.toTerm |> List.fromTerm |> Option.get |> List.map (Nat.fromTerm >> Option.get)
        Assert.AreEqual<_>(list, result)

    [<TestMethod>]
    member __.``empty list is empty`` () =
        let result = apps [ List.isEmptyT ; List.emptyT ] |> Eval.eval
        Assert.AreEqual<_>(Bool.trueT, result)

    [<TestMethod>]
    member __.``non-empty list is not empty`` () =
        let result = apps [ List.isEmptyT ; apps [ List.consT ; Nat.toTerm 5 ; List.emptyT ]] |> Eval.eval
        Assert.AreEqual<_>(Bool.falseT, result)

    [<TestMethod>]
    member __.``tryHead works on an empty list`` () =
        let list = [1..5] |> List.rev |> List.map Nat.toTerm |> List.toTerm
        let result = (List.tryHeadT $ list) |> Eval.eval
        let expected = 5 |> Nat.toTerm |> Some |> Option.toTerm
        Assert.AreEqual<_>(expected, result)

    [<TestMethod>]
    member __.``tryHead works on a non-empty list`` () =
        let list = [] |> List.rev |> List.map Nat.toTerm |> List.toTerm
        let result = (List.tryHeadT $ list) |> Eval.eval
        let expected = None |> Option.toTerm
        Assert.AreEqual<_>(expected, result)

    [<TestMethod>]
    member __.``both ways of constructing a list are equivalent`` () =
        let list = [1..5] |> List.rev |> List.map Nat.toTerm
        let l1 = list |> List.toTerm

        let rec constructListT =
            function
            | [] -> List.emptyT
            | t::ts -> apps [ List.consT ; t ; constructListT ts ]

        let l2 = list |> constructListT |> Eval.eval

        Assert.AreEqual<_>(l1, l2)

    [<TestMethod>]
    member __.``sum of 0..5 is 15`` () =
        let list = [1..5] |> List.rev |> List.map Nat.toTerm |> List.toTerm
        let result = (List.sumT $ list) |> Eval.eval
        Assert.AreEqual<_>(Nat.toTerm 15, result)

    [<TestMethod>]
    member __.``product of 0..5 is 15`` () =
        let list = [1..5] |> List.rev |> List.map Nat.toTerm |> List.toTerm
        let result = (List.productT $ list) |> Eval.eval
        Assert.AreEqual<_>(Nat.toTerm 120, result)

    [<TestMethod>]
    member __.``empty list has length 0`` () =
        let length = (List.lengthT $ List.emptyT) |> Eval.eval
        Assert.AreEqual<_>(Nat.zeroT, length)

    [<TestMethod>]
    member __.``list of seven items has length 7`` () =
        let list = List.init 7 (fun _ -> Nat.toTerm 4) |> List.toTerm
        let length = (List.lengthT $ list) |> Eval.eval
        Assert.AreEqual<_>(Nat.toTerm 7, length)

    [<TestMethod>]
    member __.``test repeat`` () =
        let result = apps [ List.repeatT ; Nat.toTerm 8 ; Nat.toTerm 3 ] |> Eval.eval
        let expected = List.replicate 8 (Nat.toTerm 3) |> List.toTerm
        Assert.AreEqual<_>(expected, result)

    [<TestMethod>]
    member __.``test append`` () =
        let l1 = [1..5] |> List.rev |> List.map Nat.toTerm
        let l2 = [6..10] |> List.map Nat.toTerm
        let result = apps [ List.appendT ; List.toTerm l1 ; List.toTerm l2 ] |> Eval.eval
        Assert.AreEqual<_>((l1 @ l2) |> List.toTerm, result)

    [<TestMethod>]
    member __.``test collect`` () =
        let list = [1..5] |> List.map Nat.toTerm
        let result = apps [ List.collectT ; (List.repeatT $ Nat.toTerm 3) ; list |> List.toTerm ] |> Eval.eval
        let expected = list |> List.collect (List.replicate 3) |> List.toTerm
        Assert.AreEqual<_>(expected, result)

    [<TestMethod>]
    member __.``test map`` () =
        let list = [1..5]
        let toTerm = List.map Nat.toTerm >> List.toTerm
        let result = (List.mapT $ toTerm list $ Nat.succT) |> Eval.eval
        let expected = list |> List.map (fun n -> n + 1) |> toTerm
        Assert.AreEqual<_>(expected, result)
