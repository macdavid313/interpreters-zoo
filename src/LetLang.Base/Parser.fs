namespace LetLang.Base

open System
open FParsec

module Ast =
    type Expression =
        | ConstExpr of int
        | DiffExpr of Expression * Expression
        | ZeroExpr of Expression
        | IfExpr of Expression * Expression * Expression
        | VarExpr of string
        | LetExpr of Expression * Expression * Expression
        | ProcExpr of Expression * Expression
        | CallExpr of Expression * Expression

    type Program = AProgram of Expression

module Parser =
    open Ast

    type UserState = unit // doesn't have to be unit, of course

    type Parser<'t> = Parser<'t, UserState>

    let isKeyword x =
        match x with
        | VarExpr s -> [| "let"; "if"; "then"; "else"; "zero?"; "proc" |] |> Array.contains s
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
        pipe3 (pstring "let" >>. pvar) (pchar '=' >>. pexpr) (pstring "in" >>. pexpr)
            (fun ident expr body -> LetExpr(ident, expr, body))

    let pproc =
        pipe2 (pstring "proc" >>. spaces >>. betweenParen pexpr) pexpr (fun ident body -> ProcExpr(ident, body))

    let pcall = betweenParen (pexpr .>>. pexpr) |>> CallExpr

    do pexprRef := spaces >>. choice [ pconst; pdiff; pzero; pif; pvar; plet; pproc; pcall ] .>> spaces

    let pprogram = spaces >>. pexpr .>> spaces .>> eof |>> AProgram

    exception ParsingException of string * ParserError * UserState

    let scanAndParse code =
        match run pprogram code with
        | Success(expr, _, _) -> expr
        | Failure(msg, e, s) -> raise (ParsingException(msg, e, s))
