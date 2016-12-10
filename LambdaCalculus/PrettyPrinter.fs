namespace LambdaCalculus

type PrettyPrinter = PrettyPrinter of (Term -> ((Term -> string) -> string) option) list

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PrettyPrinter =

    let empty = PrettyPrinter []

    let extend printer (PrettyPrinter printers) = PrettyPrinter (printer::printers)

    let makePrinter (PrettyPrinter printers) =
        let rec print t =
            match printers |> Seq.tryPick ((|>) t) with
            | None   -> failwith "No way to print this term!"
            | Some f -> f print
        print

    let minimal =

        let printAll (t : Term) (print : Term -> string) =
            match t with
            | Var i -> sprintf "%i" i
            | App (t1, t2) ->
                let printFirst t  = match t with App _ -> print t | Var _ -> print t | _ -> sprintf "(%s)" (print t)
                let printSecond t = match t with Var _ -> print t | _ -> sprintf "(%s)" (print t)
                sprintf "%s %s" (printFirst t1) (printSecond t2)
            | Lam t ->
                match t with Lam _ -> sprintf "λ%s" (print t) | _ -> sprintf "λ %s" (print t)

        PrettyPrinter [ fun t -> printAll t |> Some ]

    let standard =

        let boolPrinter   = Bool.fromTerm   >> Option.map (fun b _ -> sprintf "%b" b)
        let natPrinter    = Nat.fromTerm    >> Option.map (fun i _ -> sprintf "N%i" i)
        let pairPrinter   = Pair.fromTerm   >> Option.map (fun (f, s) p -> sprintf "(%s, %s)" (p f) (p s))
        let listPrinter   = List.fromTerm   >> Option.map (fun l p -> l |> List.map p |> String.concat " ; " |> sprintf "[ %s ]")
        let optionPrinter = Option.fromTerm >> Option.map (fun o p -> match o with None -> "None" | Some t -> sprintf "Some %s" (p t))

        minimal
        |> extend boolPrinter
        |> extend natPrinter
        |> extend pairPrinter
        |> extend listPrinter
        |> extend optionPrinter
