namespace rec PureParse.Examples

open System.Text
open PureParse

#nowarn "40"

module Arithmetic =
    begin

        (*               
            NumberExpression:
                Float  
            BaseExpression:            
                ( Expression )
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

        type Arithmetic =
            | Number of double
            | UnaryPlus of Arithmetic
            | UnaryMinus of Arithmetic
            | Add of Arithmetic * Arithmetic
            | Subtract of Arithmetic * Arithmetic
            | Multiply of Arithmetic * Arithmetic
            | Divide of Arithmetic * Arithmetic

        let rec eval body =
            match body with
            | Number n -> n
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

        let parseNumberExpression : Parser<unit, Arithmetic> =
            parse {
                let! r = parseReal<unit, double> ()
                return Number r
            }

        let parseBaseExpression : Parser<unit, Arithmetic> =
            parseNumberExpression
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

