module ParserTests

open System
open Xunit

open LetLang.Base.Ast
open LetLang.Base.Parser

[<Fact>]
let ``My test``() = Assert.True(true)

[<Fact>]
let ``Parser const``() =
    match scanAndParse "10" with
    | AProgram(ConstExpr s) -> Assert.Equal(10, s)
    | _ -> Assert.True(false)

[<Fact>]
let ``Parser diff``() =
    match scanAndParse "-( -10, -20  )" with
    | AProgram(expr) -> Assert.Equal(DiffExpr(ConstExpr(-10), ConstExpr(-20)), expr)
    match scanAndParse "-(55, -(x, 11))" with
    | AProgram(expr) -> Assert.Equal(DiffExpr(ConstExpr 55, DiffExpr(VarExpr "x", ConstExpr 11)), expr)

[<Fact>]
let ``Parser zero``() =
    match scanAndParse "zero?(x)" with
    | AProgram(expr) -> Assert.Equal(ZeroExpr(VarExpr "x"), expr)

[<Fact>]
let ``Parser if``() =
    match scanAndParse "if x then -(yy, 1) else -(z,  -1)" with
    | AProgram(IfExpr(expr1, expr2, expr3)) ->
        Assert.Equal(expr1, VarExpr "x")
        Assert.Equal(expr2, DiffExpr(VarExpr "yy", ConstExpr 1))
        Assert.Equal(expr3, DiffExpr(VarExpr "z", ConstExpr -1))
    | _ -> Assert.True(false)

[<Fact>]
let ``Parser var``() =
    [| "x"; "xyz"; "pred?"; "x-y-z"; "x_y_z" |]
    |> Array.map (fun s ->
        match scanAndParse s with
        | AProgram(expr) -> Assert.Equal(expr, VarExpr(s)))
    |> ignore
// illegal identifier

[<Fact>]
let ``Parser let``() =
    match scanAndParse "let  x = -(y, 1)  in   -(x,          y)  " with
    | AProgram(expr) ->
        Assert.Equal(expr, LetExpr(VarExpr "x", DiffExpr(VarExpr "y", ConstExpr 1), DiffExpr(VarExpr "x", VarExpr "y")))

[<Fact>]
let ``Parser proc and call``() =
    match scanAndParse "let f = proc (x) -(x, 11) in (f (f 10))" with
    | AProgram(expr) ->
        Assert.Equal
            (expr,
             LetExpr
                 (VarExpr "f", ProcExpr(VarExpr "x", DiffExpr(VarExpr "x", ConstExpr 11)),
                  CallExpr(VarExpr "f", CallExpr(VarExpr "f", ConstExpr 10))))
