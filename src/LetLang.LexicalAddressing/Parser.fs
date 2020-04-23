namespace LetLang.LexicalAddressing

open System
open FParsec

module Ast =
    type Identifier = string

    // Nameless Var/Let/Proc
    type Expression =
        | ConstExpr of int
        | DiffExpr of Expression * Expression
        | ZeroExpr of Expression
        | IfExpr of Expression * Expression * Expression
        | VarExpr of string
        | NamelessVarExpr of int
        | LetExpr of Expression * Expression * Expression
        | NamelessLetExpr of Expression * Expression
        | ProcExpr of Expression * Expression
        | NamelessProcExpr of Expression
        | CallExpr of Expression * Expression

        override this.ToString() =
            match this with
            | ConstExpr n -> n.ToString()
            | DiffExpr(expr1, expr2) -> sprintf "(- %s %s)" (expr1.ToString()) (expr2.ToString())
            | ZeroExpr expr0 -> sprintf "(zero? %s)" (expr0.ToString())
            | IfExpr(expr0, expr1, expr2) ->
                sprintf "(if %s :then %s :else %s)" (expr0.ToString()) (expr1.ToString()) (expr2.ToString())
            | VarExpr var -> var
            | NamelessVarExpr n -> sprintf "#%d" n
            | LetExpr(variable, value, body) ->
                sprintf "(let ([%s %s]) %s)" (variable.ToString()) (value.ToString()) (body.ToString())
            | NamelessLetExpr(expr, body) -> sprintf "(%%let %s %s)" (expr.ToString()) (body.ToString())
            | ProcExpr(variable, body) -> sprintf "(lambda (%s) %s)" (variable.ToString()) (body.ToString())
            | NamelessProcExpr(body) -> sprintf "(%%lexproc %s)" (body.ToString())
            | CallExpr(rator, rand) -> sprintf "(%s %s)" (rator.ToString()) (rand.ToString())

    type Program = AProgram of Expression

module Parser =
    open Ast

    type UserState = unit // doesn't have to be unit, of course

    type Parser<'t> = Parser<'t, UserState>

    let allKeywords =
        Collections.Generic.HashSet<string>([| "let"; "in"; "if"; "then"; "else"; "zero?"; "proc"; "letrec" |])

    let isKeyword x =
        match x with
        | VarExpr s -> allKeywords.Contains s
        | _ -> false

    let betweenParen p = between (pchar '(') (pchar ')') p

    let pexpr, pexprRef = createParserForwardedToRef()

    let pconst = spaces >>. pint32 .>> spaces |>> ConstExpr

    let pdiff =
        pipe2 (pstring "-(" >>. pexpr) (pchar ',' >>. pexpr .>> pchar ')') (fun expr1 expr2 -> DiffExpr(expr1, expr2))

    let pzero = pstring "zero?" >>. betweenParen pexpr |>> ZeroExpr

    let pif =
        pipe3 (pstring "if" >>. pexpr) (pstring "then" >>. pexpr) (pstring "else" >>. pexpr)
            (fun expr1 expr2 expr3 -> IfExpr(expr1, expr2, expr3))

    let pvar: Parser<_> =
        let pidentifier =
            spaces >>. asciiLetter
            .>>. many (satisfy Char.IsLetterOrDigit <|> pchar '-' <|> pchar '_' <|> pchar '?') .>> spaces |>> fun (c, cs) ->
                let chars = c :: cs
                let sb = Text.StringBuilder(chars.Length)
                chars |> List.iter (sb.Append >> ignore)
                VarExpr(sb.ToString())

        fun stream ->
            let state = stream.State
            let reply = pidentifier stream
            if reply.Status <> Ok || not (isKeyword reply.Result) then
                reply
            else
                stream.BacktrackTo(state)
                Reply(Error, expected "identifier")

    let plet =
        pipe3 (pstring "let" >>. spaces1 >>. pvar) (pchar '=' >>. pexpr) (pstring "in" >>. pexpr)
            (fun ident expr body -> LetExpr(ident, expr, body))

    let pproc =
        pipe2 (pstring "proc" >>. spaces >>. betweenParen pexpr) pexpr (fun ident body -> ProcExpr(ident, body))

    let pcall = betweenParen (pexpr .>>. pexpr) |>> CallExpr

    do pexprRef := spaces >>. choice [ pconst; pvar; pdiff; pzero; pif; pproc; pcall; plet ] .>> spaces

    let pprogram = spaces >>. pexpr .>> spaces .>> eof |>> AProgram

    exception ParsingException of string * ParserError * UserState

    let scanAndParse code =
        match run pprogram code with
        | Success(expr, _, _) -> expr
        | Failure(msg, e, s) -> raise (ParsingException(msg, e, s))
