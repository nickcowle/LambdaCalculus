namespace LambdaCalculus

open FParsec
open System.Reflection

module BaseLibrary =

    let loadModule (moduleName : string) =

        let readLamResource resourceName =
            let assembly = Assembly.GetExecutingAssembly ()
            use stream = assembly.GetManifestResourceStream resourceName
            let parsed = runParserOnStream (Parser.definitions 'λ') () resourceName stream System.Text.Encoding.UTF8

            match parsed with
            | Success (terms, _, _) -> terms
            | Failure _ -> failwithf "Could not parse file %s" resourceName

        let makeContextForModule (moduleName : string) =
            moduleName |> sprintf "%s.lam" |> readLamResource
            |> List.map (fun (termName, term) -> sprintf "%s.%s" moduleName termName, term)

        makeContextForModule moduleName

    let load () =
        let assembly = Assembly.GetExecutingAssembly()
        assembly.GetManifestResourceNames ()
        |> Seq.choose (fun s -> if s.EndsWith ".lam" then s.Substring(0, s.Length - 4) |> Some else None)
        |> Seq.collect loadModule
        |> Map.ofSeq
