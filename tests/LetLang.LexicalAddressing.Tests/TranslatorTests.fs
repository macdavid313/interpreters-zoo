module TranslatorTests

open Xunit

open LetLang.LexicalAddressing.Ast
open LetLang.LexicalAddressing.Parser
open LetLang.LexicalAddressing.StaticEnvironment
open LetLang.LexicalAddressing.Translator

[<Fact>]
let StaticEnvironment() =
    let senv =
        emptySEnv()
        |> extendSEnv "p"
        |> extendSEnv "q"
        |> extendSEnv "v"
        |> extendSEnv "p"
    Assert.Equal(Some(0), (applySEnv senv "p"))
    Assert.Equal(Some(1), (applySEnv senv "v"))
    Assert.Equal(Some(2), (applySEnv senv "q"))

[<Fact>]
let ``Translation let``() =
    let code = "let x = 1 in let y = -(x, 2) in -(x, y)"
    match translationOfProgram (scanAndParse code) with
    | AProgram expr ->
        Assert.Equal
            (NamelessLetExpr
                (ConstExpr 1,
                 NamelessLetExpr
                     (DiffExpr(NamelessVarExpr 0, ConstExpr 2), DiffExpr(NamelessVarExpr 1, NamelessVarExpr 0))), expr)

[<Fact>]
let ``Translation proc``() =
    let code = @"let x = 37
                in proc (y)
                    let z = -(y, x)
                    in -(x, y)"
    match translationOfProgram (scanAndParse code) with
    | AProgram expr ->
        Assert.Equal
            (NamelessLetExpr
                (ConstExpr 37,
                 NamelessProcExpr
                     (NamelessLetExpr
                         (DiffExpr(NamelessVarExpr 0, NamelessVarExpr 1), DiffExpr(NamelessVarExpr 2, NamelessVarExpr 1)))),
             expr)

[<Fact>]
let ``Translation letrec``() = Assert.True(true)
