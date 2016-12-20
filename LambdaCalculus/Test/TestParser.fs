namespace LambdaCalculus.Test

open FParsec
open LambdaCalculus
open NUnit.Framework
open System.Reflection

[<TestFixture>]
type TestParser () =

    let assertAreEqual (expected : 'a) (actual : 'a) =
        Assert.AreEqual(expected, actual)

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
                assertAreEqual identifier parsed
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
            match run (Parser.term 'λ') s with
            | ParserResult.Success (t, _, _) -> Some t
            | _ -> None

        let variable = "0"
        let result = parse variable |> Option.get |> Eval.resolveIdentifiers Map.empty |> Result.force
        assertAreEqual (Var 0) result

        let app = "3 1"
        let result = parse app |> Option.get |> Eval.resolveIdentifiers Map.empty |> Result.force
        assertAreEqual (appVars [3 ; 1]) result

    [<Test>]
    member __.``test named variables`` () =

        let parse s =
            match run (ParserV.term 'λ') s with
            | ParserResult.Success (t, _, _) -> Some t
            | _ -> None

        let compose = "λ f λ g λ x . g (f x)"

        match parse compose with
        | None -> Assert.Fail ()
        | Some t ->
            assertAreEqual Combinators.composeT (t |> TermIV.deBruijn |> Eval.resolveIdentifiers Map.empty |> Result.force)

    [<Test>]
    member __.``test parsing an identifier`` () =

        let parse s =
            match run (ParserV.term 'λ') s with
            | ParserResult.Success (t, _, _) -> Some t
            | _ -> None

        let ident = "foo"

        match parse ident with
        | None -> Assert.Fail ()
        | Some t ->
            assertAreEqual (VarIV ident) t

    [<Test>]
    member __.``test parsing a qualified identifier`` () =

        let parse s =
            match run (ParserV.term 'λ') s with
            | ParserResult.Success (t, _, _) -> Some t
            | _ -> None

        let ident = "foo.bar"

        match parse ident with
        | None -> Assert.Fail ()
        | Some t ->
            assertAreEqual (IdentIV ident) t
