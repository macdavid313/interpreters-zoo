namespace LetLang.Base

open System

open Ast
open Parser
// open Values
//open Environment
open Runtime

module Eval =

    exception WrongTypeException of string

    let reportWrongType expectType expr =
        let msg = sprintf "Expect type '%s' from '%s'" expectType (expr.ToString())
        raise (WrongTypeException(""))

    exception NoBindingException of string

    let reportNoBinding var = raise (NoBindingException(sprintf "No binding found for variable '%s'" var))

    let rec valueOf expr env =
        match expr with
        | ConstExpr num -> NumVal num
        | DiffExpr(expr1, expr2) ->
            match valueOf expr1 env with
            | NumVal val1 ->
                match valueOf expr2 env with
                | NumVal val2 -> NumVal(val1 - val2)
                | _ -> reportWrongType "Int" expr2
            | _ -> reportWrongType "Int" expr1
        | ZeroExpr(expr1) ->
            match valueOf expr1 env with
            | NumVal val1 ->
                if val1 = 0 then BoolVal true else BoolVal false
            | _ -> reportWrongType "Int" expr1
        | IfExpr(expr0, expr1, expr2) ->
            match valueOf expr0 env with
            | BoolVal val0 ->
                if val0 then valueOf expr1 env else valueOf expr2 env
            | _ -> reportWrongType "Bool" expr0
        | VarExpr var ->
            match applyEnv env var with
            | Some expval -> expval
            | None -> reportNoBinding var
        | LetExpr(VarExpr var, expr1, body) ->
            let newEnv = extendEnv var (valueOf expr1 env) env
            valueOf body newEnv
        | ProcExpr(VarExpr var, expr1) -> ProcVal(var, expr1, ref (env))
        | CallExpr(rator, rand) ->
            let arg = valueOf rand env
            match valueOf rator env with
            | ProcVal(var, body, pEnv) -> valueOf body (extendEnv var arg pEnv.Value)
            | _ -> reportWrongType "Procedure" rator
        | LetrecExpr(VarExpr name, VarExpr var, body, letrecBody) ->
            valueOf letrecBody (extendEnvRec name var body env)
        | _ -> Void

    let valueOfProgram pgm =
        match pgm with
        | AProgram expr -> valueOf expr (initEnv())

    let run code = valueOfProgram (scanAndParse code)
