module RuntimeTests

open Xunit

open LetLang.LexicalAddressing.Runtime

[<Fact>]
let Environment() =
    let env =
        emptyNamelessEnv()
        |> extendNamelessEnv (NumVal 3)
        |> extendNamelessEnv (NumVal 2)
        |> extendNamelessEnv (NumVal 1)
    Assert.Equal(NumVal 1, applyNamelessEnv env 0)
    Assert.Equal(NumVal 2, applyNamelessEnv env 1)
    Assert.Equal(NumVal 3, applyNamelessEnv env 2)
