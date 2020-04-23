module EvalTests

open Xunit

open LetLang.LexicalAddressing.Runtime
open LetLang.LexicalAddressing.Eval

[<Fact>]
let ``Eval const``() =
    Assert.Equal(NumVal 10, run "10")
    Assert.Equal(NumVal 0, run "0")
    Assert.Equal(NumVal -10, run "-10")

[<Fact>]
let ``Eval diff``() =
    let code = "-(1, 10)"
    Assert.Equal(NumVal -9, run code)

[<Fact>]
let ``Eval zero``() =
    Assert.Equal(BoolVal true, run "zero?(-(1, 1))")
    Assert.Equal(BoolVal false, run "zero?(-(1, -1))")

[<Fact>]
let ``Eval let``() =
    let code = "let x = 5 in let y = -(x, 10) in -(x, y)"
    Assert.Equal(NumVal 10, run code)

[<Fact>]
let ``Eval if``() =
    let code = "let x = 10 in if zero?(-(x, 10)) then -(x, -10) else x"
    Assert.Equal(NumVal 20, run code)
    let code = "let x = 10 in if zero?(-(x, 5)) then -(x, -10) else x"
    Assert.Equal(NumVal 10, run code)

[<Fact>]
let ``Eval proc``() =
    let code = "let f = proc (x) -(x, 11) in (f (f 10))"
    Assert.Equal(NumVal -12, run code)
    let code = "(proc (f) (f (f 10)) proc (x) -(x, 11))"
    Assert.Equal(NumVal -12, run code)
    let code = "let addBy = proc (n) proc (x) -(x, -(0, n)) in let addBy10 = (addBy 10) in (addBy10 10)"
    Assert.Equal(NumVal 20, run code)
