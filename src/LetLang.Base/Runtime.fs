namespace LetLang.Base

open Ast

module Runtime =

    type ExpVal =
        | NumVal of int
        | BoolVal of bool
        | ProcVal of Identifier * Expression * ref<Env>
        | Void

        override this.ToString() =
            match this with
            | NumVal n -> n.ToString()
            | BoolVal x ->
                if x then "#t" else "#f"
            | ProcVal(var, body, _) -> sprintf "(λ (%s) (%s))" var (body.ToString())
            | Void -> "#<void>"

    and Env = Map<Identifier, ExpVal>

    let emptyEnv(): Env = Map<Identifier, ExpVal>(Seq.empty)

    let extendEnv variable value (env: Env): Env = env.Add(variable, value)

    let extendEnvRec pName pVar pBody (env: Env): Env =
        let pEnvRef = ref (emptyEnv())
        let newEnv = extendEnv pName (ProcVal(pVar, pBody, pEnvRef)) env
        pEnvRef.Value <- newEnv
        newEnv

    let applyEnv (env: Env) variable = env.TryFind(variable)

    let initEnv() =
        emptyEnv()
        |> extendEnv "i" (NumVal 1)
        |> extendEnv "v" (NumVal 5)
        |> extendEnv "x" (NumVal 10)

    exception WrongTypeException of string

    let reportWrongType expectType expr =
        let msg = sprintf "Expect type '%s' from '%s'" expectType (expr.ToString())
        raise (WrongTypeException(msg))

    exception NoBindingException of string

    let reportNoBinding var = raise (NoBindingException(sprintf "No binding found for variable '%s'" var))
