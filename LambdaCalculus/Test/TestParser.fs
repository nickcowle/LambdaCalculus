namespace LambdaCalculus.Test

open FParsec
open LambdaCalculus
open NUnit.Framework
open System.Reflection

[<TestFixture>]
type TestParser () =

    [<Test>]
    member __.``identifier parses letters and digits`` () =

        let identifierParser = Parser.identifier

        let shouldParse =
            [
                "compose"
                "Compose2"
                "t1234"
                "t12T34T"
            ]

        for identifier in shouldParse do
            let result = run identifierParser identifier
            match result with
            | ParserResult.Success (parsed, _, _) ->
                Assert.AreEqual(identifier, parsed)
            | _ -> Assert.Fail ()

        let shouldntParse =
            [
                ""
                "5"
                "1test"
            ]

        for identifier in shouldntParse do
            let result = run identifierParser identifier
            match result with
            | ParserResult.Failure _ -> ()
            | _ -> Assert.Fail ()

    [<Test>]
    member __.``test term parsing`` () =

        let parse s =
            match run Parser.term s with
            | ParserResult.Success (t, _, _) -> Some t
            | _ -> None

        let variable = "0"
        let result = parse variable |> Option.get |> Eval.resolveIdentifiers Map.empty |> Result.force
        Assert.AreEqual(Var 0, result)

        let app = "3 1"
        let result = parse app |> Option.get |> Eval.resolveIdentifiers Map.empty |> Result.force
        Assert.AreEqual(appVars [3 ; 1], result)
