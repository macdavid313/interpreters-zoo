namespace LetLang.Base

module Values =

    type ExpVal =
        | NumVal of int
        | BoolVal of bool
        | ProcVal of string * Ast.Expression * Map<string, ExpVal>
        | Void

        override this.ToString() =
            match this with
            | NumVal n -> n.ToString()
            | BoolVal x ->
                if x then "#t" else "#f"
            | ProcVal(var, body, _) -> sprintf "(λ (%s) (%s))" var (body.ToString())
            | Void -> "#<void>"
