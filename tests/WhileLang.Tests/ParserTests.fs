module ParserTests

open Xunit

open WhileLang.Parser.Ast
open WhileLang.Parser.Parse

[<Fact>]
let Lexer() =
    let code = @"fact := 1 ;
    val := 10000 ;
    cur := val ;
    mod := 1000000007 ;

    while ( cur > 1 )
      do
       {
          fact := fact * cur ;
          fact := fact - fact / mod * mod ;
          cur := cur - 1
       } ;

    cur := 0"
    Assert.Equal
        (Sequence
            (Assignment(Variable "fact", IntLiteral 1UL),
             Sequence
                 (Assignment(Variable "val", IntLiteral 10000UL),
                  Sequence
                      (Assignment(Variable "cur", VariableExpr(Variable "val")),
                       Sequence
                           (Assignment(Variable "mod", IntLiteral 1000000007UL),
                            Sequence
                                (WhileLoop
                                    (ArithmeticComp(VariableExpr(Variable "cur"), GreaterThan, IntLiteral 1UL),
                                     Sequence
                                         (Assignment
                                             (Variable "fact",
                                              ArithmeticOp
                                                  (VariableExpr(Variable "fact"), Mult, VariableExpr(Variable "cur"))),
                                          Sequence
                                              (Assignment
                                                  (Variable "fact",
                                                   ArithmeticOp
                                                       (VariableExpr(Variable "fact"), Subtract,
                                                        ArithmeticOp
                                                            (ArithmeticOp
                                                                (VariableExpr(Variable "fact"), Divide,
                                                                 VariableExpr(Variable "mod")), Mult,
                                                             VariableExpr(Variable "mod")))),
                                               Assignment
                                                   (Variable "cur",
                                                    ArithmeticOp(VariableExpr(Variable "cur"), Subtract, IntLiteral 1UL))))),
                                 Assignment(Variable "cur", IntLiteral 0UL)))))), parse code)

    let code = @"a := 10 ;
    b := 100 ;

    if ( a < b ) then
        {
            min := a ;
            max := b
        }
    else {
        min := b ;
        max := a
        }"
    Assert.Equal
        (Sequence
            (Assignment(Variable "a", IntLiteral 10UL),
             Sequence
                 (Assignment(Variable "b", IntLiteral 100UL),
                  IfBranch
                      (ArithmeticComp(VariableExpr(Variable "a"), LessThan, VariableExpr(Variable "b")),
                       Sequence
                           (Assignment(Variable "min", VariableExpr(Variable "a")),
                            Assignment(Variable "max", VariableExpr(Variable "b"))),
                       Sequence
                           (Assignment(Variable "min", VariableExpr(Variable "b")),
                            Assignment(Variable "max", VariableExpr(Variable "a")))))), parse code)
