namespace LambdaCalculus.Test

open FParsec
open LambdaCalculus
open NUnit.Framework
open System.Reflection

[<TestFixture>]
type TestBaseLibrary () =

    let filesToTest =
        [
            "Combinators",
            [
                "compose",     Combinators.composeT
                "compose2",    Combinators.compose2T
                "Y",           Combinators.Y
                "bindToApply", Combinators.bindToApplyT
                "applyToMap",  Combinators.applyToMapT
            ]

            "Bool",
            [
                "true",  Bool.trueT
                "false", Bool.falseT
                "and",   Bool.andT
                "or",    Bool.orT
                "not",   Bool.notT
                "if",    Bool.ifT
            ]

            "Pair",
            [
                "make",   Pair.makeT
                "first",  Pair.firstT
                "second", Pair.secondT
                "flip",   Pair.flipT
            ]

            "Ordering",
            [
                "lessThan",    Ordering.lessThanT
                "equal",       Ordering.equalT
                "greaterThan", Ordering.greaterThanT
            ]

            "Nat",
            [
                "zero",     Nat.zeroT
                "succ",     Nat.succT
                "add",      Nat.addT
                "mult",     Nat.multT
                "isZero",   Nat.isZeroT
                "pred",     Nat.predT
                "subtract", Nat.subtractT
                "equals",   Nat.equalsT
                "compare",  Nat.compareT
            ]

            "Option",
            [
                "none",   Option.noneT
                "some",   Option.someT
                "bind",   Option.bindT
                "return", Option.returnT
                "apply",  Option.applyT
                "map",    Option.mapT
                "isNone", Option.isNoneT
                "isSome", Option.isSomeT
            ]

            "List",
            [
                "empty",     List.emptyT
                "cons",      List.consT
                "isEmpty",   List.isEmptyT
                "replicate", List.replicateT
                "repeat",    List.repeatT
                "singleton", List.singletonT
                "fold",      List.foldT
                "length",    List.lengthT
                "append",    List.appendT
                "collect",   List.collectT
                "bind",      List.bindT
                "return",    List.returnT
                "apply",     List.applyT
                "map",       List.mapT
                "head",      List.headT
                "sum",       List.sumT
                "product",   List.productT
                "take",      List.takeT
                "skip",      List.skipT
                "nats",      List.natsT
            ]

            "Map",
            [
                "empty",  Map.emptyT
                "node",   Map.nodeT
                "insert", Map.insertT
                "find",   Map.findT
            ]
        ]

    let context =
        filesToTest
        |> Seq.collect (fst >> BaseLibrary.loadModule)
        |> Seq.map (fun (name, t) -> name, TermI.removeSelfReference name t)
        |> Map.ofSeq

    [<Test>]
    member __.``test correctness of base library definition`` () =

        let testModule (moduleName : string, expectedTerms : (string * Term) list) =

            let testTerm (termName : string, expected : Term) =
                let fullName = sprintf "%s.%s" moduleName termName
                let term =
                    match Map.tryFind fullName context with
                    | Some t -> t
                    | None -> failwithf "Could not find %s in context" fullName
                let actual = term |> Eval.resolveIdentifiers context |> Result.force
                let errorMessage =
                    let p = PrettyPrinter.minimal |> PrettyPrinter.makePrinter
                    sprintf "Definitions of %s do not match! Expected:\n%s\nbut got:\n%s" fullName (p expected) (p actual)
                Assert.AreEqual(expected, actual, errorMessage)

            expectedTerms |> List.iter testTerm

        filesToTest |> List.iter testModule
