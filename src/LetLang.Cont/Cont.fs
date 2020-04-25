namespace LetLang.Cont

open LetLang.Base.Ast
open LetLang.Base.Runtime

module Continuation =

    type Cont =
        | EndCont
        | ZeroCont of Cont
        | Diff1Cont of Expression * Env * Cont
        | Diff2Cont of ExpVal * Cont
        | LetCont of var: Identifier * body: Expression * env: Env * k: Cont
        | IfCont of thenExpr: Expression * elseExpr: Expression * env: Env * k: Cont
        | RatorCont of Expression * Env * Cont
        | RandCont of ExpVal * Cont
