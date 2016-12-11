open FParsec
open LambdaCalculus
open System

let banner = "
  (Ob.         `7MM\"\"\"Mq.  `7MM\"\"\"YMM  `7MM\"\"\"Mq.`7MMF'
     M           MM   `MM.   MM    `7    MM   `MM. MM
     db          MM   ,M9    MM   d      MM   ,M9  MM
    AMA.         MMmmdM9     MMmmMM      MMmmdM9   MM
   AM'`M         MM  YM.     MM   Y  ,   MM        MM
  ,M'  db        MM   `Mb.   MM     ,M   MM        MM     ,M
 JM'    Yb.    .JMML. .JMM..JMMmmmmMMM .JMML.    .JMMmmmmMMM
                                                             "

type Command =
| Quit
| Help
| Eval of TermI
| Print of TermI

let commandParser : _ Parser =
    let ws = many1 (skipChar ' ')
    let termParser = Parser.term '\\'

    let quit  = skipString ":q" |>> (fun () -> Quit)
    let help  = skipString ":h" |>> (fun () -> Help)
    let print = skipString ":p" >>. ws >>. termParser |>> Print
    let eval  = termParser |>> Eval
    choice [ quit ; help ; print ; eval ] .>> eof

let printHelp () =
    printfn ":h - Display this help"
    printfn ":q - Quit"
    printfn ":p - Print a lambda term without evaluating it"
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

let print t =
    match t with
    | IdentI s ->
        match Map.tryFind s context with
        | None -> printfn "Could not find definition for identifier %s" s
        | Some t -> printfn "%s" (PrettyPrinter.printTermI t)
    | _ -> printfn "Print can only be used on identifiers"

let runCommand (command : string) =
    let parse = run commandParser command
    match parse with
    | ParserResult.Success (c, _, _) ->
        match c with
        | Quit    -> printfn "Exiting." ; Environment.Exit 0
        | Help    -> printHelp ()
        | Eval  t -> evaluate t
        | Print t -> print t
    | ParserResult.Failure (error, _, _) -> printfn "Failed to parse command:\n%s" error

[<EntryPoint>]
let main argv =

    Console.OutputEncoding <- Text.Encoding.UTF8
    printfn "%s" banner
    printfn " Lambda Calculus REPL (type :h for help.)"

    while true do
        printfn ""
        printf "> "
        let command = Console.ReadLine ()
        runCommand command

    0
