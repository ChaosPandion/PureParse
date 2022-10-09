namespace rec PureParse.Examples

open System
open System.Numerics
open System.Text
open PureParse

#nowarn "40"

module Number =

    (* 
        Digit:
            0-9
        NonZeroDigit:
            1-9
        BinaryDigit:
            0-1
        HexDigit:
            Digit
            a-f
            A-F
        BinarySpecifier:
            b
            B
        HexSpecifier:
            x
            X
        BinaryDigits:
            BinaryDigit
            BinaryDigit BinaryDigits
        HexDigits:
            HexDigit
            HexDigit HexDigits
        Digits:
            Digit
            Digit Digits
        BinaryInteger:
            0 BinarySpecifier BinaryDigits
        HexInteger:
            0 HexSpecifier HexDigits
        DecimalInteger:
            0
            NonZeroDigit Digits?
        Fractional:
            . Digits
        ExponentSpecifier:
            e
            E
        Exponent:
            ExponentSpecifier Digits
            ExponentSpecifier + Digits
            ExponentSpecifier - Digits
        Sign:
            +
            -
        IntegerBody:
            DecimalInteger
            HexInteger
            BinaryInteger
        Integer:
            IntegerBody
            Sign IntegerBody
        FloatBody:
            DecimalInteger
            DecimalInteger Fractional
            DecimalInteger Exponent
            DecimalInteger Fractional Exponent 
        Float:
            FloatBody
            Sign FloatBody
    *)

    let private digitToInt = function
        | x when x >= '0' && x <= '9' -> int x - int '0'
        | x when x >= 'a' && x <= 'f' -> 10 + int x - int 'a'
        | x when x >= 'A' && x <= 'F' -> 10 + int x - int 'A'
        | _ -> failwith ""

    let private powInt32 n (b:int32) =
        let rec loop i limit x = 
            if i = limit 
            then x 
            else loop (i + 1) limit (x * b)
        loop 0 (n - 1) 1

    let private powInt64 n (b:int64) =
        let rec loop i limit x = 
            if i = limit 
            then x 
            else loop (i + 1L) limit (x * b)
        loop 0L (n - 1L) 1L
            
    let private powBigInteger (n:BigInteger) (b:BigInteger) =         
        let rec loop i limit x = 
            if i = limit 
            then x 
            else loop (i + 1I) limit (x * b)
        loop 0I (n - 1I) 1I
     
    let private powDouble (n:double) (b:double) =
        let rec loop i limit x = 
            if i = limit 
            then x 
            else loop (i + 1.0) limit (x * b)
        loop 0.0 (n - 1.0) 1.0

    let rec private parseIntegerRunes = function
        | [] -> 0.0
        | c::cs -> (digitToInt (toChar c) |> double) * (10.0 ** double cs.Length) + parseIntegerRunes cs

    let rec private parseIntegerRunesWithBase b = function
        | [] -> 0L
        | c::cs -> int64(digitToInt (toChar c)) * (pown b cs.Length) + parseIntegerRunesWithBase b cs

    let private parseDigit<'TState> : Parser<'TState, Rune> = 
        charRange '0' '9'

    let private parseNonZeroDigit<'TState> : Parser<'TState, Rune> = 
        charRange '1' '9'

    let private parseBinaryDigit<'TState> : Parser<'TState, Rune> = 
        charRange '0' '1'

    let private parseHexDigit<'TState> : Parser<'TState, Rune> = 
        parseDigit <|> charRange 'a' 'f' <|> charRange 'A' 'F'

    let private parseBinarySpecifier<'TState> : Parser<'TState, Rune> = 
        parseRune (Rune('b')) <|> parseRune (Rune('B'))

    let private parseHexSpecifier<'TState> : Parser<'TState, Rune> = 
        parseRune (Rune('x')) <|> parseRune (Rune('x'))

    let private parseBinaryDigits<'TState> : Parser<'TState, List<Rune>> = 
        parseList parseBinaryDigit (Many 1)

    let private parseHexDigits<'TState> : Parser<'TState, List<Rune>> = 
        parseList parseHexDigit (Many 1)

    let private parseDigits<'TState> : Parser<'TState, List<Rune>> = 
        parseList parseDigit (Many 1)

    let private parseBinaryInteger<'TState> : Parser<'TState, int64> = 
        parse {
            do! skipChar '0'
            do! skip parseBinarySpecifier
            let! digits = parseBinaryDigits
            let integer = parseIntegerRunesWithBase 2 digits
            return integer
        }

    let private parseHexInteger<'TState> : Parser<'TState, int64> = 
        parse {
            do! skipChar '0'
            do! skip parseHexSpecifier
            let! digits = parseHexDigits
            let integer = parseIntegerRunesWithBase 16 digits
            return integer
        }

    let private parseDecimalInteger<'TState> : Parser<'TState, int64> = 
        parse {
            return! parse {
                do! skipChar '0'
                return 0L
            } <|> parse {
                let! digit = parseNonZeroDigit
                let! digits = parseList parseDigit (Many 0)
                let integer = parseIntegerRunesWithBase 10 (digit::digits)
                return integer
            }
        }

    let rec private parseInteger1 = function
        | [] -> 0.0
        | c::cs -> 
            (digitToInt c |> double) * (10.0 ** double cs.Length) + parseInteger1 cs
    let rec private parseSign = function
        | Some '-' -> -1.0
        | Some '+' | None -> 1.0
        | Some _ -> 
            failwith "Unexpected Sign"

    let private sign<'TState> : Parser<'TState, char option> = opt (parseChar '-' <|> parseChar '+')
    let private exponentChar = RuneCharSeq "eE"

    let private parseExponentPart<'TState> : Parser<'TState, double> =
        parse {
            let! _ = parseAnyOf exponentChar
            let! s = sign
            let! d = parseInt32
            let signModifier = parseSign s;
            let power = signModifier * (double d)
            let result = 10.0 ** power
            return result
        }

    let private parseFractionalPart<'TState> : Parser<'TState, double> =
        parse {
            do! skipChar '.'
            let! d = parseDigits
            let n = double <| parseIntegerRunesWithBase 10 d
            let pow = -(d.Length);
            let result = n * pown 10.0 pow
            return double result
        }

    let private parseIntegerPart<'TState> : Parser<'TState, int * double> =
        parse {
            let! sign = opt (parseChar '-' <|> parseChar '+')
            let! digits = parseInt32
            let signModifier = parseSign sign
            return digits, signModifier
        }

    let private parseInteger<'TState> : Parser<'TState, int64> =        
        parse {
            let! sign = opt (parseChar '-' <|> parseChar '+')
            let! integer = chooseSync [ parseBinaryInteger; parseHexInteger; parseDecimalInteger ]
            match sign with
            | Some '-' -> 
                return -1L * integer
            | _ -> 
                return integer
        }

    let private parseFloat<'TState> : Parser<'TState, double> =        
        parse {
            let! ip, sign = parseIntegerPart  
            let! fp = parseFractionalPart <|> result 0.0
            let! ep = parseExponentPart <|> result 1.0
            let x = double ip + fp
            let signed = sign * x
            let n = signed * ep
            return n
        }

    let parseInt64 text = 
        match tryRun parseInteger text () with
        | RunSuccess(_, value, _) -> value
        | _ -> failwith("Failed to parse.")

    let parseDouble text = 
        match tryRun parseFloat text () with
        | RunSuccess(_, value, _) -> value
        | _ -> failwith("Failed to parse.")
