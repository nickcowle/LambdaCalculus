namespace LambdaCalculus

open FParsec

type 'a Parser = Parser<'a, unit>

module Parser =

    let identifier : _ Parser =
        let isIdentifierFirstChar c = isLetter c
        let isIdentifierChar c = isLetter c || isDigit c
        many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"

    let qualifiedIdentifier =
        identifier .>>. pstring "." .>>. identifier |>> (fun ((s1, s2), s3) -> s1 + s2 + s3)

    let rec term =

        let term, termRef = createParserForwardedToRef ()

        let ws1 = skipMany1 (skipChar ' ')

        let apps =
            let variable = pint32 |>> VarI
            let bracketed = between (skipChar '(') (skipChar ')') term
            let identifier = qualifiedIdentifier |>> IdentI
            let applicant = variable <|> bracketed <|> identifier
            sepBy1 applicant ws1 |>> appsI

        let lams =
            many1 (skipChar 'λ') |>> List.length .>> ws1 .>>. apps |>> ((<||) lamsI)

        termRef := lams <|> apps
        term

    let definition =
        let ws = skipMany (skipChar ' ')
        identifier .>> ws .>> pstring ":=" .>> ws .>>. term

    let definitions =
        sepBy definition (skipNewline >>. skipNewline) .>> eof
