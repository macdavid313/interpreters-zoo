namespace LetLang.LexicalAddressing

open Ast

module Runtime =

    type ExpVal =
        | NumVal of int
        | BoolVal of bool
        | ProcVal of Expression * NamelessEnv
        | Void

        override this.ToString() =
            match this with
            | NumVal n -> n.ToString()
            | BoolVal x ->
                if x then "#t" else "#f"
            | ProcVal(body, _) -> sprintf "(λ (#) (%s))" (body.ToString())
            | Void -> "#<void>"

    and NamelessEnv = ExpVal list

    let emptyNamelessEnv(): NamelessEnv = []

    let extendNamelessEnv value (env: NamelessEnv): NamelessEnv = value :: env

    let applyNamelessEnv (env: NamelessEnv) idx = env.Item(idx)
