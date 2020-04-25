namespace LetLang.Cont

open LetLang.Base.Ast
open LetLang.Base.Parser
open LetLang.Base.Runtime

open Continuation

module Eval =

    let rec valueOf (expr: Expression) (env: Env) (k: Cont) =
        match expr with
        | ConstExpr n -> applyCont k (NumVal n)
        | DiffExpr(expr1, expr2) -> valueOf expr1 env (Diff1Cont(expr2, env, k))
        | ZeroExpr expr0 -> valueOf expr0 env (ZeroCont k)
        | IfExpr(expr1, expr2, expr3) -> valueOf expr1 env (IfCont(expr2, expr3, env, k))
        | VarExpr var ->
            match applyEnv env var with
            | Some(expval) -> applyCont k expval
            | None -> reportNoBinding var
        | LetExpr(VarExpr var, expr0, body) -> valueOf expr0 env (LetCont(var, body, env, k))
        | ProcExpr(VarExpr var, body) -> applyCont k (ProcVal(var, body, ref (env)))
        | CallExpr(rator, rand) -> valueOf rator env (RatorCont(rand, env, k))
        | LetrecExpr(VarExpr name, VarExpr var, body, letrecBody) ->
            valueOf letrecBody (extendEnvRec name var body env) k
        | _ -> raise RuntimeException

    and applyCont (cont: Cont) (expval: ExpVal) =
        match cont with
        | EndCont ->
            printfn "End of computation."
            expval
        | ZeroCont k ->
            match expval with
            | NumVal n -> applyCont k (BoolVal(n = 0))
            | _ -> reportWrongType "Int" expval
        | Diff1Cont(expr2, env, k) -> valueOf expr2 env (Diff2Cont(expval, k))
        | Diff2Cont(expval1, k) ->
            match expval1 with
            | NumVal val1 ->
                match expval with
                | NumVal val2 -> applyCont k (NumVal(val1 - val2))
                | _ -> reportWrongType "Int" expval
            | _ -> reportWrongType "Int" expval1
        | IfCont(thenExpr, elseExpr, env, k) ->
            match expval with
            | BoolVal t ->
                if t then valueOf thenExpr env k else valueOf elseExpr env k
            | _ -> reportWrongType "Bool" expval
        | LetCont(var, body, env, k) -> valueOf body (extendEnv var expval env) k
        | RatorCont(rand, env, k) -> valueOf rand env (RandCont(expval, k))
        | RandCont(expval1, k) ->
            match expval1 with
            | ProcVal(var, body, env) -> valueOf body (extendEnv var expval env.Value) k
            | _ -> reportWrongType "Procedure" expval1

    let valueOfProgram (AProgram pgm) = valueOf pgm (initEnv()) EndCont

    let run = scanAndParse >> valueOfProgram
