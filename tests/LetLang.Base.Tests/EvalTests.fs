module EvalTests

open System
open Xunit

open LetLang.Base.Values
open LetLang.Base.Eval

[<Fact>]
let ``Eval let``() =
    let code = "let x = 5 in let y = -(x, 10) in -(x, y)"
    Assert.Equal(run code, NumVal 10)

[<Fact>]
let ``Eval proc``() =
    let code = "let f = proc (x) -(x, 11) in (f (f 10))"
    Assert.Equal(run code, NumVal -12)
    let code = "(proc (f) (f (f 10)) proc (x) -(x, 11))"
    Assert.Equal(run code, NumVal -12)
