namespace LetLang.Base

open Values

module Environment =
    type Env = Map<string, ExpVal>

    let emptyEnv(): Env = Map<string, ExpVal>(Seq.empty)

    let extendEnv variable value (env: Env): Env = env.Add(variable, value)

    let applyEnv (env: Env) (variable: string): ExpVal option = env.TryFind(variable)

    let initEnv() =
        emptyEnv()
        |> extendEnv "i" (NumVal 1)
        |> extendEnv "v" (NumVal 5)
        |> extendEnv "x" (NumVal 10)
