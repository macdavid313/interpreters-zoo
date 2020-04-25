namespace LetLang.Cont

open System

open Eval

module Repl =

    [<EntryPoint>]
    let rec main argv =
        let input = ReadLine.Read("λ> ")
        if input.Length <> 0 then
            try
                let value = run input
                Console.WriteLine(value.ToString())
            with e -> Console.WriteLine(e.ToString())
            main argv
        else
            main argv
