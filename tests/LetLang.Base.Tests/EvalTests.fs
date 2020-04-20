module EvalTests

open System
open Xunit

open LetLang.Base.Values
open LetLang.Base.Eval

[<Fact>]
let ``Eval const``() =
    Assert.Equal(run "10", NumVal 10)
    Assert.Equal(run "0", NumVal 0)
    Assert.Equal(run "-10", NumVal -10)

[<Fact>]
let ``Eval diff``() =
    let code = "-(1, 10)"
    Assert.Equal(run code, NumVal -9)

[<Fact>]
let ``Eval zero``() =
    Assert.Equal(run "zero?(-(1, 1))", BoolVal true)
    Assert.Equal(run "zero?(-(1, -1))", BoolVal false)

[<Fact>]
let ``Eval variable``() =
    Assert.Equal(run "i", NumVal 1)
    Assert.Equal(run "v", NumVal 5)
    Assert.Equal(run "x", NumVal 10)

[<Fact>]
let ``Eval let``() =
    let code = "let x = 5 in let y = -(x, 10) in -(x, y)"
    Assert.Equal(run code, NumVal 10)

[<Fact>]
let ``Eval if``() =
    let code = "let x = 10 in if zero?(-(x, 10)) then -(x, -10) else x"
    Assert.Equal(run code, NumVal 20)
    let code = "let x = 10 in if zero?(-(x, 5)) then -(x, -10) else x"
    Assert.Equal(run code, NumVal 10)

[<Fact>]
let ``Eval proc``() =
    let code = "let f = proc (x) -(x, 11) in (f (f 10))"
    Assert.Equal(run code, NumVal -12)
    let code = "(proc (f) (f (f 10)) proc (x) -(x, 11))"
    Assert.Equal(run code, NumVal -12)
