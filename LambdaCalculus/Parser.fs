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

    let ws1 = skipMany1 (skipChar ' ')

    let term lambdaChar =

        let term, termRef = createParserForwardedToRef ()

        let apps =
            let variable = pint32 |>> VarI
            let bracketed = between (skipChar '(') (skipChar ')') term
            let identifier = qualifiedIdentifier |>> IdentI
            let applicant = variable <|> bracketed <|> identifier
            sepBy1 applicant ws1 |>> appsI

        let lams =
            many1 (skipChar lambdaChar) |>> List.length .>> ws1 .>>. apps |>> ((<||) lamsI)

        termRef := lams <|> apps
        term

    let definition lambdaChar =
        let ws = skipMany (skipChar ' ')
        identifier .>> ws .>> pstring ":=" .>> ws .>>. term lambdaChar

    let definitions lambdaChar =
        sepBy (definition lambdaChar) (skipNewline >>. skipNewline) .>> eof


module ParserV =

    let term lambdaChar =

        let term, termRef = createParserForwardedToRef ()

        let apps =
            let bracketed = between (skipChar '(') (skipChar ')') term
            let identifier = Parser.qualifiedIdentifier |>> IdentIV
            let variable = Parser.identifier |>> VarIV
            // Important to backtrack on identifier as it might be a variable
            let applicant = bracketed <|> attempt identifier <|> variable
            sepBy1 applicant Parser.ws1 |>> appsIV

        let lams =
            many1 (skipChar lambdaChar >>. Parser.ws1 >>. Parser.identifier .>> Parser.ws1) .>> skipChar '.' .>> Parser.ws1 .>>. apps |>> ((<||) lamsIV)

        termRef := lams <|> apps
        term

    let definition lambdaChar =
        let ws = skipMany (skipChar ' ')
        Parser.identifier .>> ws .>> pstring ":=" .>> ws .>>. term lambdaChar

    let definitions lambdaChar =
        sepBy (definition lambdaChar) (skipNewline >>. skipNewline) .>> eof
