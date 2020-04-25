namespace LetLang.LexicalAddressing

open Ast
open Parser
open Translator
open Runtime

module Eval =

    exception WrongTypeException of string

    let reportWrongType expectType expr =
        let msg = sprintf "Expect type '%s' from '%s'" expectType (expr.ToString())
        raise (WrongTypeException(msg))

    exception RuntimeException

    let rec valueOf expr (env: NamelessEnv) =
        match expr with
        | ConstExpr n -> NumVal n
        | DiffExpr(expr1, expr2) ->
            match valueOf expr1 env with
            | NumVal val1 ->
                match valueOf expr2 env with
                | NumVal val2 -> NumVal(val1 - val2)
                | _ -> reportWrongType "Int" expr2
            | _ -> reportWrongType "Int" expr1
        | ZeroExpr expr0 ->
            match valueOf expr0 env with
            | NumVal n -> BoolVal(n = 0)
            | _ -> reportWrongType "Int" expr0
        | IfExpr(expr1, expr2, expr3) ->
            match valueOf expr1 env with
            | BoolVal t when t -> valueOf expr2 env
            | BoolVal _ -> valueOf expr3 env
            | _ -> reportWrongType "Bool" expr1
        | CallExpr(rator, rand) ->
            let arg = valueOf rand env
            match valueOf rator env with
            | ProcVal(body, pEnv) -> valueOf body (extendNamelessEnv arg pEnv)
            | _ -> reportWrongType "Procedure" rator
        | NamelessVarExpr idx -> applyNamelessEnv env idx
        | NamelessLetExpr(expr0, body) ->
            let value = valueOf expr0 env
            valueOf body (extendNamelessEnv value env)
        | NamelessProcExpr body -> ProcVal(body, env)
        | _ -> raise RuntimeException

    let valueOfProgram (AProgram pgm) = valueOf pgm (emptyNamelessEnv())

    let run =
        scanAndParse
        >> translationOfProgram
        >> valueOfProgram
