namespace LambdaCalculus

open FParsec

module Parser =

    let identifier () =
        let isIdentifierFirstChar c = isLetter c
        let isIdentifierChar c = isLetter c || isDigit c
        many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"

    let rec term () =

        let term, termRef = createParserForwardedToRef ()

        let ws1 = skipMany1 (skipChar ' ')

        let apps =
            let variable = pint32 |>> Var
            let bracketed = between (skipChar '(') (skipChar ')') term
            let applicant = variable <|> bracketed
            sepBy1 applicant ws1 |>> Constructors.apps

        let lams =
            many1 (skipChar 'λ') |>> List.length .>> ws1 .>>. apps |>> ((<||) Constructors.lams)

        termRef := lams <|> apps
        term

    let definition () =
        let ws = skipMany (skipChar ' ')
        identifier () .>> ws .>> pstring ":=" .>> ws .>>. term ()

    let definitions () =
        sepBy (definition ()) (skipNewline >>. skipNewline) .>> eof
