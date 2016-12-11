open FParsec
open LambdaCalculus
open System

type Command =
| Quit
| Help
| Eval of TermI

let commandParser : _ Parser =
    let quit = skipString ":q" |>> (fun () -> Quit)
    let help = skipString ":h" |>> (fun () -> Help)
    let eval = Parser.term |>> Eval
    choice [ quit ; help ; eval ] .>> eof

let printHelp () =
    printfn ":h - Display this help"
    printfn ":q - Quit"
    printfn ""
    printfn "Type a lambda term to have it evaluated."

let context = BaseLibrary.load ()

let printer = PrettyPrinter.minimal |> PrettyPrinter.makePrinter

let evaluate t =
    let t = t |> Eval.resolveIdentifiers context
    match t with
    | Success t -> t |> Eval.eval |> printer |> printfn "%s"
    | Failure ids ->
        printfn
            "Cannot evaluate term - the following identifier(s) could not be resolved: %s"
            (String.concat ", " ids)

let runCommand (command : string) =
    let command = command.Replace('\\', 'λ')
    let parse = run commandParser command
    match parse with
    | ParserResult.Success (c, _, _) ->
        match c with
        | Quit   -> printfn "Exiting." ; Environment.Exit 0
        | Help   -> printHelp ()
        | Eval t -> evaluate t
    | ParserResult.Failure (error, _, _) -> printfn "Failed to parse command:\n%s" error

[<EntryPoint>]
let main argv =

    Console.OutputEncoding <- Text.Encoding.UTF8
    printfn "Lambda Calculus REPL"
    printfn "type :h for help."

    while true do
        printfn ""
        printf "> "
        let command = Console.ReadLine ()
        runCommand command

    0
