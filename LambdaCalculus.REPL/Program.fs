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
| Eval   of TermI
| Print  of TermI
| Define of (string * TermI)
| PrintState

let commandParser =
    let ws = many1 (skipChar ' ')
    let termParser = Parser.term '\\'

    let quit  = skipString ":q" |>> (fun () -> Quit)
    let help  = skipString ":h" |>> (fun () -> Help)
    let print = skipString ":p" >>. ws >>. termParser |>> Print
    let eval  = termParser |>> Eval
    let defn  = skipString ":d" >>. ws >>. Parser.definition '\\' |>> Define
    let state = skipString ":s" |>> (fun () -> PrintState)
    choice [ quit ; help ; print ; eval ; defn ; state ] .>> eof

let printHelp () =
    printfn ":h - Display this help"
    printfn ":q - Quit"
    printfn ":p - Print a lambda term without evaluating it"
    printfn ":d name := term - Define a term"
    printfn ":s - Print state (user-defined terms)"
    printfn ""
    printfn "Type a lambda term to have it evaluated."

let context     = ref (BaseLibrary.load ())
let userContext = ref Map.empty

let printer = PrettyPrinter.minimal |> PrettyPrinter.makePrinter

let evaluate t =
    let t = t |> Eval.resolveIdentifiers context.Value
    match t with
    | Success t -> t |> Eval.eval |> printer |> printfn "%s"
    | Failure ids ->
        printfn
            "Cannot evaluate term - the following identifier(s) could not be resolved: %s"
            (String.concat ", " ids)

let print t =
    match t with
    | IdentI s ->
        match Map.tryFind s context.Value with
        | None -> printfn "Could not find definition for identifier %s" s
        | Some t -> printfn "%s" (PrettyPrinter.printTermI t)
    | _ -> printfn "Print can only be used on identifiers"

let define (name : string, t) =
    let fullName = sprintf "User.%s" name
    context     := Map.add fullName t context.Value
    userContext := Map.add fullName t userContext.Value
    printfn "Term %s now defined." fullName

let printState () =
    let printDefinition (name, term) =
        printfn "%s := %s" name (PrettyPrinter.printTermI term)
        printfn ""

    userContext.Value |> Map.toSeq |> Seq.iter printDefinition

let runCommand (command : string) =
    let parse = run commandParser command
    match parse with
    | ParserResult.Success (c, _, _) ->
        match c with
        | Quit       -> printfn "Exiting." ; Environment.Exit 0
        | Help       -> printHelp ()
        | Eval  t    -> evaluate t
        | Print t    -> print t
        | Define t   -> define t
        | PrintState -> printState ()
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
