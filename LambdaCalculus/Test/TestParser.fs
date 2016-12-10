namespace LambdaCalculus.Test

open FParsec
open LambdaCalculus
open NUnit.Framework
open System.Reflection

[<TestFixture>]
type TestParser () =

    let readLamResource resourceName =
        let assembly = Assembly.GetExecutingAssembly()
        use stream = assembly.GetManifestResourceStream resourceName
        let parsed = runParserOnStream (Parser.definitions ()) () resourceName stream System.Text.Encoding.UTF8

        match parsed with
        | Success (terms, _, _) -> terms
        | Failure _ -> failwith "Could not parse file"

    [<Test>]
    member __.``identifier parses letters and digits`` () =

        let identifierParser = Parser.identifier ()

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
            | Success (parsed, _, _) ->
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
            | Failure _ -> ()
            | _ -> Assert.Fail ()

    [<Test>]
    member __.``test term parsing`` () =

        let parse s =
            match run (Parser.term ()) s with
            | Success (t, _, _) -> Some t
            | Failure _ -> None

        let variable = "0"
        let result = parse variable |> Option.get |> Eval.resolveIdentifiers Map.empty
        Assert.AreEqual(Var 0, result)

        let app = "3 1"
        let result = parse app |> Option.get |> Eval.resolveIdentifiers Map.empty
        Assert.AreEqual(appVars [3 ; 1], result)

    [<Test>]
    member __.``test parse of Bool file`` () =

        let terms =
            readLamResource "Bool.lam"
            |> List.map (fun (name, t) -> name, Eval.resolveIdentifiers Map.empty t)

        let expected =
            [
                "true",  Bool.trueT
                "false", Bool.falseT
                "and",   Bool.andT
                "or",    Bool.orT
                "not",   Bool.notT
                "if",    Bool.ifT
            ]

        List.zip expected terms |> List.iter Assert.AreEqual

    [<Test>]
    member __.``test parse of Pair file`` () =

        let terms =
            readLamResource "Pair.lam"
            |> List.map (fun (name, t) -> name, Eval.resolveIdentifiers Map.empty t)

        let expected =
            [
                "make",   Pair.makeT
                "first",  Pair.firstT
                "second", Pair.secondT
                "flip",   Pair.flipT
            ]

        List.zip expected terms |> List.iter Assert.AreEqual

    [<Test>]
    member __.``test parse of Combinators file`` () =

        let terms =
            let termsI = readLamResource "Combinators.lam"
            let context = termsI |> List.map (fun (n, t) -> sprintf "Combinators.%s" n, t) |> Map.ofSeq
            termsI |> List.map (fun (name, t) -> name, Eval.resolveIdentifiers context t)

        let expected =
            [
                "compose",     Combinators.composeT
                "compose2",    Combinators.compose2T
                "Y",           Combinators.Y
                "bindToApply", Combinators.bindToApplyT
                "applyToMap",  Combinators.applyToMapT
            ]

        List.zip expected terms |> List.iter Assert.AreEqual
