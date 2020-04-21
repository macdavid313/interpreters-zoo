namespace LetLang.Base

open System

module Runtime =

    type ExpVal =
        | NumVal of int
        | BoolVal of bool
        | ProcVal of string * Ast.Expression * ref<Env>
        | Void

        override this.ToString() =
            match this with
            | NumVal n -> n.ToString()
            | BoolVal x ->
                if x then "#t" else "#f"
            | ProcVal(var, body, _) -> sprintf "(λ (%s) (%s))" var (body.ToString())
            | Void -> "#<void>"

    and Env = Map<string, ExpVal>

    let emptyEnv(): Env = Map<string, ExpVal>(Seq.empty)

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
