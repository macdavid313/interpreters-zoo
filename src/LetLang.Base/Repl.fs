namespace LetLang.Base

open System

open Eval

module Repl =

    [<EntryPoint>]
    let rec main argv =
        let input = ReadLine.Read("λ> ")
        try
            let value = run input
            Console.WriteLine(value.ToString())
        with e -> Console.WriteLine(e.ToString())
        main argv
