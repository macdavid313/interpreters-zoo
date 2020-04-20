module EnvironmentTests

open System
open Xunit

open LetLang.Base.Ast
open LetLang.Base.Values
open LetLang.Base.Environment

[<Fact>]
let Environment() =
    let env = extendEnv "b" (NumVal 2) (extendEnv "a" (NumVal 1) (emptyEnv()))
    Assert.Equal((applyEnv env "a"), Some(NumVal 1))
    Assert.Equal((applyEnv env "b"), Some(NumVal 2))
    Assert.Equal((applyEnv env "c"), None)
