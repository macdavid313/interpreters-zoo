namespace LetLang.Base

open System

module Runtime =

    type ExpVal =
        | NumVal of int
        | BoolVal of bool
        | ProcVal of string * Ast.Expression * ref<Environment>
        | Void

        override this.ToString() =
            match this with
            | NumVal n -> n.ToString()
            | BoolVal x ->
                if x then "#t" else "#f"
            | ProcVal(var, body, _) -> sprintf "(λ (%s) (%s))" var (body.ToString())
            | Void -> "#<void>"

    and Environment = Env of Map<string, ExpVal>

    let emptyEnv() = Env(Map<string, ExpVal>(Seq.empty))

    let extendEnv variable value (Env env) = Env(env.Add(variable, value))

    let extendEnvRec pName pVar pBody (Env env) =
        let pEnv = ref (emptyEnv())
        let newEnv = extendEnv pName (ProcVal(pVar, pBody, pEnv)) (Env env)
        pEnv.Value <- newEnv
        newEnv

    let applyEnv (Env env) variable = env.TryFind(variable)

    let initEnv() =
        emptyEnv()
        |> extendEnv "i" (NumVal 1)
        |> extendEnv "v" (NumVal 5)
        |> extendEnv "x" (NumVal 10)
