namespace LetLang.Base

open Values

module Environment =
    type Env = Map<string, ExpVal>

    let emptyEnv(): Env = Map<string, ExpVal>(Seq.empty)

    let extendEnv variable value (env: Env): Env = env.Add(variable, value)

    let applyEnv (env: Env) (variable: string): ExpVal option = env.TryFind(variable)

    let initEnv() =
        let env = emptyEnv()
        let env = extendEnv "i" (NumVal 1) env
        let env = extendEnv "v" (NumVal 5) env
        extendEnv "x" (NumVal 10) env
