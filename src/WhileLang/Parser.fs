namespace WhileLang.Parser

module Ast =
    type Variable = Variable of string

    type ArithmeticOp =
        | Add
        | Subtract
        | Mult
        | Divide

    type BooleanOp =
        | And
        | Or

    type ComparisonOp =
        | LessThan
        | GreaterThan

    type ArithmeticExpr =
        | VariableExpr of Variable
        | IntLiteral of uint64
        | ArithmeticOp of ArithmeticExpr * ArithmeticOp * ArithmeticExpr

    type BooleanExpr =
        | BoolLiteral of bool
        | BooleanOp of BooleanExpr * BooleanOp * BooleanExpr
        | ArithmeticComp of ArithmeticExpr * ComparisonOp * ArithmeticExpr

    type Statement =
        | Assignment of Variable * ArithmeticExpr
        | Sequence of Statement * Statement
        | IfBranch of BooleanExpr * Statement * Statement
        | WhileLoop of BooleanExpr * Statement


module Utils =
    let flip f x y = f y x

    let flatten ((a, b), c) = (a, b, c)

    let first (a, b) = a
    let second (a, b) = b

    let charsToString (cs: char seq) = System.String.Concat(cs)
    let parseBool (s: string) = System.Convert.ToBoolean(s)

    let (??|) a b =
        match a with
        | None -> b
        | _ -> a


module Parser =
    open Utils

    type OptionState<'s, 't> = 's -> ('t * 's) option

    type Parser<'a> = Parser of OptionState<string, 'a>

    let run (Parser p) = p

    let pBind p f =
        let fn = run p >> Option.bind (fun (value, rest) -> run (f value) rest)
        Parser fn

    let pReturn x = Parser(fun s -> Some(x, s))

    let (>>=) = pBind

    let pApply pf px = pf >>= (fun f -> px >>= (f >> pReturn))

    let pMap f =
        f
        |> pReturn
        |> pApply

    let pLift2 f x y =
        pReturn f
        |> flip pApply x
        |> flip pApply y

    let (|>>) x y = pMap y x // map result
    let (|>>|) a b = a |>> (fun _ -> b) // fix result

    let (.>>.) p1 p2 = p1 >>= (fun res1 -> p2 >>= (fun res2 -> pReturn (res1, res2))) // chain

    let (<|>) (Parser p1) (Parser p2) =
        let fn s =
            let res1 = p1 s
            match res1 with
            | Some _ -> res1
            | None -> p2 s
        Parser fn // choice

    let constant x = Parser(fun s -> Some(x, s))

    let any = List.reduce (<|>)

    let sequence ps =
        let cons h t = h :: t
        List.foldBack (pLift2 cons) ps (pReturn [])

    let many p =
        let rec parseZeroOrMore parser input =
            match run parser input with
            | Some(firstVal, rest) ->
                let (restVals, rest2) = parseZeroOrMore parser rest
                (firstVal :: restVals, rest2)
            | None -> ([], input)
        Parser(Some << parseZeroOrMore p)

    let many1 p =
        (sequence
            [ p |>> List.singleton
              many p ])
        |>> List.concat

    let offset p =
        let p' = p |>> fun r -> "", r

        let rec parse s =
            let res = run p' s
            match res, s with
            | None, "" -> None
            | None, _ -> parse s.[1..] |> Option.map (fun ((skipped, res'), rest) -> (s.[0..0] + skipped, res'), rest)
            | _ -> res
        Parser parse

    let (.>>.^) p1 p2 =
        let sliceTo (s: string) n =
            if n = -1 then "" else s.[..n]

        let rec parse sOrig s =
            match run (offset p2) s with
            | None -> None // if can't parse second anywhere in string, give up
            | Some((skipped, p2Res), rest) ->
                let p1SearchStr = sliceTo sOrig (sOrig.Length - s.Length - 1) + skipped
                match run p1 p1SearchStr with
                | Some(p1Res, "") -> Some((p1Res, p2Res), rest) // if parsed second and also first before it, return
                | _ -> parse sOrig s.[skipped.Length + 1..] // if parsed second but not first, try again using a later occurrence of the second

        Parser(fun s -> parse s s) // chain but with second evaluated first

    let (.>>.^*) p1 p2 =
        let (..|) (s: string) n =
            if n = -1 then "" else s.[..n] //sliceTo

        let offsetAll p =
            let rec parse prevResults prevSkipped remaining =
                match run (offset p) remaining with
                | None -> prevResults
                | Some((skipped, res), rest) ->
                    let newSkipped = prevSkipped + (remaining ..| skipped.Length)
                    let next = (newSkipped ..| (newSkipped.Length - 2), res), rest
                    parse (next :: prevResults) newSkipped remaining.[skipped.Length + 1..]
            parse [] ""

        let parse s =
            let rec parseRec p2results (s: string) =
                match p2results with
                | [] -> None
                | ((skipped, p2Res), rest: string) :: otherResults ->
                    match run p1 skipped with
                    | Some(p1Res, "") -> Some((p1Res, p2Res), rest)
                    | _ -> parseRec otherResults s
            parseRec (offsetAll p2 s) s

        Parser parse

    // chaining syntax is [.]>[-]>[.][^[*]], where a dot indicates result on that side will be kept, a - means
    // whitespace is expected and ignored between them, and a trailing ^ means the second parser is evaluated first
    // a further * means the first parser is evaluated greedily
    let (.>>) p1 p2 = p1 .>>. p2 |>> first // chain, ignore second result
    let (>>.) p1 p2 = p1 .>>. p2 |>> second // chain, ignore first result
    let (.>>^) p1 p2 = p1 .>>.^ p2 |>> first // chain, ignore second result, evaluate second first
    let (>>.^) p1 p2 = p1 .>>.^ p2 |>> second // chain, ignore first result, evaluate second first
    let (.>>^*) p1 p2 = p1 .>>.^* p2 |>> first // chain, ignore second result, evaluate second first, greedy
    let (>>.^*) p1 p2 = p1 .>>.^* p2 |>> second // chain, ignore first result, evaluate second first, greedy


module Parsers =
    open Ast
    open Utils
    open Parser

    // PRIMITIVE PARSERS
    let pChar c =
        let fn s =
            if String.length s > 0 && s.[0] = c then Some(c, s.[1..]) else None
        Parser fn

    let pAnyChar = List.map pChar >> any

    let pInt =
        pAnyChar [ '0' .. '9' ]
        |> many1
        |>> (charsToString >> uint64)

    let pString s =
        s
        |> Seq.toList
        |> List.map pChar
        |> sequence
        |> pMap charsToString

    let pWord =
        pAnyChar [ 'a' .. 'z' ]
        |> many1
        |>> charsToString
    let pWs = pAnyChar [ ' '; '\t'; '\n'; '\r' ] |> many1

    let (.>->.) p1 p2 = p1 .>> pWs .>>. p2 // chain, whitespace-separated
    let (.>->) p1 p2 = p1 .>->. p2 |>> first // chain, whitespace-separated, ignore second result
    let (>->.) p1 p2 = p1 .>->. p2 |>> second // chain, whitespace-separated, ignore first result
    let (.>->.^) p1 p2 = (p1 .>> pWs) .>>.^ p2
    let (.>->^) p1 p2 = p1 .>->.^ p2 |>> first
    let (>->.^) p1 p2 = p1 .>->.^ p2 |>> second
    let (.>->.^*) p1 p2 = (p1 .>> pWs) .>>.^* p2
    let (.>->^*) p1 p2 = p1 .>->.^* p2 |>> first
    let (>->.^*) p1 p2 = p1 .>->.^* p2 |>> second

    // EXPRESSION PARSERS
    let pVariable = pWord |>> Variable

    let pAddSub = (pChar '+' |>>| Add) <|> (pChar '-' |>>| Subtract)
    let pMultDiv = (pChar '*' |>>| Mult) <|> (pChar '/' |>>| Divide)

    let pBooleanAnd = pString "and" |>>| And
    let pBooleanOr = pString "or" |>>| Or

    let pComparisonOp = (pChar '>' |>>| GreaterThan) <|> (pChar '<' |>>| LessThan)

    let pArithmetic =
        let rec pArithmetic' s =
            let p =
                ((Parser pArithmetic') .>->.^* pAddSub .>->. (Parser pArithmetic') |>> flatten |>> ArithmeticOp)
                <|> ((Parser pArithmetic') .>->.^* pMultDiv .>->. (Parser pArithmetic') |>> flatten |>> ArithmeticOp)
                <|> (pChar '(' >->. ((Parser pArithmetic') .>->^ pChar ')')) <|> (pVariable |>> VariableExpr)
                <|> (pInt |>> IntLiteral)
            run p s
        Parser pArithmetic'

    let pBoolean =
        let rec pBoolean' s =
            let p =
                ((Parser pBoolean') .>->.^ pBooleanOr .>->. (Parser pBoolean') |>> flatten |>> BooleanOp)
                <|> ((Parser pBoolean') .>->.^ pBooleanAnd .>->. (Parser pBoolean') |>> flatten |>> BooleanOp)
                <|> (pArithmetic .>->.^ pComparisonOp .>->. pArithmetic |>> flatten |>> ArithmeticComp)
                <|> (pChar '(' >->. ((Parser pBoolean') .>->^ pChar ')'))
                <|> ((pString "true" <|> pString "false") |>> parseBool |>> BoolLiteral)
            run p s
        Parser pBoolean'


    // STATEMENT PARSERS
    let pAssignment = pVariable .>-> pString ":=" .>->. pArithmetic |>> Assignment

    let pIfBranch pStatement' =
        pString "if" >->. (pBoolean .>->^ (pString "then" .>->. pChar '{'))
        .>->. (pStatement' .>->^ (pChar '}' .>->. pString "else" .>->. pChar '{')) .>->. (pStatement' .>->^ pChar '}')
        |>> flatten |>> IfBranch

    let pWhileLoop pStatement' =
        pString "while" >->. (pBoolean .>-> pString "do") .>-> pChar '{' .>->. (pStatement' .>->^ pChar '}')
        |>> WhileLoop

    let pStatement =
        let rec pStatement' s =
            let p =
                ((Parser pStatement') .>->^ pChar ';' .>->. (Parser pStatement') |>> Sequence)
                <|> (pIfBranch <| Parser pStatement') <|> (pWhileLoop <| Parser pStatement') <|> pAssignment
            run p s
        Parser pStatement'

module Parse =
    open Parser
    open Parsers

    let parse code =
        match run pStatement code with
        | Some(program, "") -> program
        | Some(_, s) -> failwith <| sprintf "error parsing, remaining: %s" s
        | None -> failwith "error"
