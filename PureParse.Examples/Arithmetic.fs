namespace rec PureParse.Examples

open System.Text
open PureParse

open System

#nowarn "40"

module Arithmetic =
    begin

        (*               
            NumberExpression:
                Float  
            Letter:
                a-z
                A-Z
            Digit:
                0-9
            NameStart:
                Letter
            NamePart:
                Letter
                Digit
            NameTrail:
                NamePart
                NamePart NameTrail
            NameExpression:
                NameStart
                NameStart NameTrail
            Argument:
                Expression
            ArgumentList:
                Argument
                ArgumentList , Argument
            FunctionCallExpression:
                NameExpression ( ArgumentList )
            BaseExpression:            
                ( Expression ) 
                FunctionCallExpression
                NumberExpression
            SignExpression:
                BaseExpression
                + SignExpression
                - SignExpression  
            MultiplicativeExpression:
                SignExpression
                MultiplicativeExpression * SignExpression
                MultiplicativeExpression / SignExpression 
            AdditiveExpression:
                MultiplicativeExpression
                AdditiveExpression + MultiplicativeExpression
                AdditiveExpression - MultiplicativeExpression
            Expression:
                AdditiveExpression
        *)

        let fMap:Map<string, double array -> double> = 
            Map.ofList [
                ("abs", fun args -> abs args[0])
                ("acos", fun args -> acos args[0])
                ("acosh", fun args -> Math.Acosh args[0])
                ("asin", fun args -> asin args[0])
                ("asinh", fun args -> Math.Asinh args[0])
                ("atan", fun args -> atan args[0])
                ("atan2", fun args -> atan2 args[0] args[1])
                ("cbrt", fun args -> Math.Cbrt args[0])
                ("ceil", fun args -> ceil args[0])
                ("cos", fun args -> cos args[0])
                ("cosh", fun args -> cosh args[0])
                ("exp", fun args -> exp args[0])
                ("floor", fun args -> floor args[0])
                ("log", fun args -> log args[0])
                ("log10", fun args -> log10 args[0])
                ("pow", fun args -> pown args[0] (args[1] |> int))
                ("sin", fun args -> sin args[0])
                ("sinh", fun args -> sinh args[0])
                ("sqrt", fun args -> sqrt args[0])
                ("tan", fun args -> tan args[0])
                ("tanh", fun args -> tanh args[0])
                ("truncate", fun args -> truncate args[0])
            ]

        type Arithmetic =
            | Number of double
            | FunctionCall of string * Arithmetic list
            | UnaryPlus of Arithmetic
            | UnaryMinus of Arithmetic
            | Add of Arithmetic * Arithmetic
            | Subtract of Arithmetic * Arithmetic
            | Multiply of Arithmetic * Arithmetic
            | Divide of Arithmetic * Arithmetic

        let rec eval body =
            match body with
            | Number n -> n
            | FunctionCall (name, args) ->
                let args = args |> List.map eval |> Array.ofList
                fMap[name] args
            | UnaryPlus e -> +(eval e)
            | UnaryMinus e -> -(eval e)
            | Add (left, right) -> eval left +  eval right
            | Subtract (left, right) -> eval left - eval right
            | Multiply (left, right) -> eval left * eval right
            | Divide (left, right) -> eval left / eval right

        let middle (_, y, _) = y

        let ws = skipWhiteSpace<unit> ()

        let skipOperator c = 
            sequence3 ws (parseChar c) ws ||> fun _ -> ()
            
        let parsePlusOperator:Parser<unit, char> = 
            (sequence3 ws (parseChar '+') ws) ||> middle

        let parseMinusOperator:Parser<unit, char> = 
            (sequence3 ws (parseChar '-') ws) ||> middle

        let parseMultiplyOperator:Parser<unit, char> = 
            (sequence3 ws (parseChar '*') ws) ||> middle

        let parseDivideOperator:Parser<unit, char> = 
            (sequence3 ws (parseChar '/') ws) ||> middle

        let parseDigit: Parser<unit, Rune> = satisfy Rune.IsDigit

        let parseLetter: Parser<unit, Rune> = satisfy (fun r -> (r >= Rune('a') && r <= Rune('z')) || (r >= Rune('A') && r <= Rune('Z')))

        let parseLetterOrDigit: Parser<unit, Rune> = parseLetter <|> parseDigit

        let parseNameStart = parseLetter

        let parseNamePart = parseLetterOrDigit

        let parseNameTrail: Parser<unit, Rune list> = parseMany parseNamePart 0

        let parseName: Parser<unit, string> = 
            parse {
                let! s = parseNameStart
                let! rest = optional parseNameTrail
                match rest with
                | Some rest ->
                    return s.ToString() + String.Join("", rest)
                | None -> 
                    return s.ToString()
            }

        let parseArguments: Parser<unit, Arithmetic list> = 
            parseManySep (parseExpression) (skipOperator ',') false 0

        let parseFunctionExpression: Parser<unit, Arithmetic> =
            parse {
                let! n = parseName
                let! _, args, _ =  sequence3 (skipOperator '(') parseArguments (skipOperator ')')
                return FunctionCall (n, args)
            }

        let parseNumberExpression : Parser<unit, Arithmetic> =
            parse {
                let! r = parseReal<unit, double> ()
                return Number r
            }

        let parseBaseExpression : Parser<unit, Arithmetic> =
            parseNumberExpression <|>
            parseFunctionExpression
                <|> parse {
                    let! _, r, _ = sequence3 (skipOperator '(') parseExpression (skipOperator ')')
                    return r
                }

        let rec parseSignExpression (x:voption<Arithmetic>) : Parser<unit, Arithmetic> =
                parse {
                    let! op = optional (parsePlusOperator <|> parseMinusOperator) 
                    match op, x with
                    | Some '+', z -> 
                        let! e = parseSignExpression z
                        return UnaryPlus e
                    | Some '-', z -> 
                        let! e = parseSignExpression z
                        return UnaryMinus e
                    | _, ValueSome e ->
                        return e
                    | _, ValueNone -> 
                        return! parseBaseExpression
                }

        let rec parseMultiplicativeExpression (x:voption<Arithmetic>) : Parser<unit, Arithmetic> =
            parse {
                let! op = optional (parseMultiplyOperator <|> parseDivideOperator) 
                match op, x with
                | Some '*', ValueSome x ->
                    let! y = parseSignExpression ValueNone
                    let z = Multiply (x, y)
                    return! parseMultiplicativeExpression (ValueSome z)
                | Some '/', ValueSome x ->
                    let! y = parseSignExpression ValueNone
                    let z = Divide (x, y)
                    return! parseMultiplicativeExpression (ValueSome z)
                | _, ValueNone ->
                    let! x = parseSignExpression ValueNone
                    return! parseMultiplicativeExpression (ValueSome x)
                | _, ValueSome e ->
                    return e
            }

        let rec parseAdditiveExpression (x:voption<Arithmetic>) : Parser<unit, Arithmetic> =
            parse {
                let! op = optional (parsePlusOperator <|> parseMinusOperator) 
                match op, x with
                | Some '+', ValueSome x ->
                    let! y = parseMultiplicativeExpression ValueNone
                    let z = Add (x, y)
                    return! parseAdditiveExpression (ValueSome z)
                | Some '-', ValueSome x ->
                    let! y = parseMultiplicativeExpression ValueNone
                    let z = Subtract (x, y)
                    return! parseAdditiveExpression (ValueSome z)
                | _, ValueNone ->
                    let! x = parseMultiplicativeExpression ValueNone
                    return! parseAdditiveExpression (ValueSome x)
                | _, ValueSome e ->
                    return e
            } 

        let parseExpression : Parser<unit, Arithmetic> = (sequence3 ws (parseAdditiveExpression ValueNone) ws) ||> middle

        let parseText text = 
            tryRun parseExpression text ()

        let evalText text =
            match parseText text with
            | RunSuccess (_, r, _) -> 
                eval r
            | RunFailure (_, e, _) -> 
                raise e

    end

