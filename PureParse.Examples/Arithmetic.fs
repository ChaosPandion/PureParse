namespace rec PureParse.Examples

open System.Text
open PureParse

#nowarn "40"

module Arithmetic =
    begin

        (*
            WhiteSpace:
                SP
                TB
                NL
                CR NL
            Expression:
                AdditiveExpression
                ( AdditiveExpression )
            AdditiveExpression:
                MultiplicativeExpression
                AdditiveExpression + MultiplicativeExpression
                AdditiveExpression - MultiplicativeExpression
            MultiplicativeExpression:
                NumberExpression
                MultiplicativeExpression * NumberExpression
                MultiplicativeExpression / NumberExpression
            NumberExpression:
                Integer
        *)

        type Arithmetic =
            | Number of double
            | Add of Arithmetic * Arithmetic
            | Subtract of Arithmetic * Arithmetic
            | Multiply of Arithmetic * Arithmetic
            | Divide of Arithmetic * Arithmetic

        let rec eval body =
            match body with
            | Number n -> n
            | Add (Number left, Number right) -> left + right
            | Add (Number left, right) -> left + eval right
            | Add (left, Number right) -> eval left + right
            | Add (left, right) -> eval left +  eval right
            | Subtract (Number left, Number right) -> left - right
            | Subtract (Number left, right) -> left - eval right
            | Subtract (left, Number right) -> eval left - right
            | Subtract (left, right) -> eval left - eval right
            | Multiply (Number left, Number right) -> left * right
            | Multiply (Number left, right) -> left * eval right
            | Multiply (left, Number right) -> eval left * right
            | Multiply (left, right) -> eval left * eval right
            | Divide (Number left, Number right) -> left / right
            | Divide (Number left, right) -> left / eval right
            | Divide (left, Number right) -> eval left / right
            | Divide (left, right) -> eval left / eval right

        let parseNumberExpression () : Parser<unit, Arithmetic> =
            parse {
                let! r = PureParse.Examples.Number.parseFloat<unit>
                return Number r
            }

        let parseMultiplicativeExpression () : Parser<unit, Arithmetic> =
           parse {
                let! x = parseMultiplicativeExpression ()
                do! skipWhiteSpace ()
                do! skipChar '*'
                do! skipWhiteSpace ()
                let! y = parseNumberExpression ()
                return Multiply (x, y)
            } <|> parse {
                let! x = parseMultiplicativeExpression ()
                do! skipWhiteSpace ()
                do! skipChar '/'
                do! skipWhiteSpace ()
                let! y = parseNumberExpression ()
                return Divide (x, y)
            } <|>  parse {
                return! parseNumberExpression ()
            } 

        let parseAdditiveExpression () : Parser<unit, Arithmetic> =
            parse {
                let! x = parseAdditiveExpression ()
                do! skipWhiteSpace ()
                do! skipChar '+'
                do! skipWhiteSpace ()
                let! y = parseMultiplicativeExpression ()
                return Add (x, y)
            } <|> parse {
                let! x = parseAdditiveExpression ()
                do! skipWhiteSpace ()
                do! skipChar '-'
                do! skipWhiteSpace ()
                let! y = parseMultiplicativeExpression ()
                return Subtract (x, y)
            } <|>  parse {
                return! parseMultiplicativeExpression ()
            }

        let parseExpression () : Parser<unit, Arithmetic> =
            parse {
                do! skipWhiteSpace ()
                do! skipChar '('
                do! skipWhiteSpace ()
                let! r = parseAdditiveExpression ()
                do! skipWhiteSpace ()
                do! skipChar ')'
                do! skipWhiteSpace ()
                return r
            } <|> parse {
                do! skipWhiteSpace ()
                let! r = parseAdditiveExpression ()
                do! skipWhiteSpace ()
                return r
            }

        let parseText text = 
            tryRun (parseExpression ()) text ()

        let evalText text =
            match parseText text with
            | RunSuccess (_, r, _) -> eval r
            | RunFailure (_, e, _) -> raise e

    end

