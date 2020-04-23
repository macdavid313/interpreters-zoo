namespace LetLang.LexicalAddressing

// The environment for the translation (compile) time
module StaticEnvironment =
    type StaticEnv = string list

    let emptySEnv(): StaticEnv = []

    let extendSEnv variable (senv: StaticEnv): StaticEnv = variable :: senv

    let rec applySEnv (senv: StaticEnv) variable =
        match senv with
        | [] -> None
        | v :: _ when v = variable -> Some(0)
        | _ :: vs ->
            match applySEnv vs variable with
            | Some(addr) -> Some(1 + addr)
            | _ -> None

    exception StaticEnvironmentNoBindingException

module Translator =

    open Ast
    open StaticEnvironment

    exception TranslationInvalidExpressionException of Expression

    let rec translationOf expr senv =
        match expr with
        | ConstExpr _ -> expr
        | DiffExpr(expr1, expr2) -> DiffExpr(translationOf expr1 senv, translationOf expr2 senv)
        | ZeroExpr expr1 -> ZeroExpr(translationOf expr1 senv)
        | IfExpr(expr1, expr2, expr3) ->
            IfExpr(translationOf expr1 senv, translationOf expr2 senv, translationOf expr3 senv)
        | VarExpr var ->
            match (applySEnv senv var) with
            | Some(idx) -> NamelessVarExpr idx
            | None -> raise StaticEnvironmentNoBindingException
        | LetExpr(VarExpr var, expr0, body) ->
            NamelessLetExpr(translationOf expr0 senv, translationOf body (extendSEnv var senv))
        | ProcExpr(VarExpr var, body) -> NamelessProcExpr(translationOf body (extendSEnv var senv))
        | CallExpr(rator, rand) -> CallExpr(translationOf rator senv, translationOf rand senv)
        | _ -> raise (TranslationInvalidExpressionException expr)

    let translationOfProgram (AProgram pgm) = AProgram(translationOf pgm (emptySEnv()))
