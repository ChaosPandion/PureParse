namespace rec PureParse.Examples

#nowarn "40";

module PLisp =

    
    (*
        WhiteSpace:
            WS
            TAB
            CR
            NL  

        OpenList: WhiteSpace ( WhiteSpace

        CloseList: WhiteSpace ) WhiteSpace

        SpecialNameChar:
            !
            @
            #
            $
            ^
            &
            *
            -
            +
            =
            ?
            /
            <
            >
            ~
            `
            _
            [
            ]
            ;
            :

        NameLetterChar:
            a-z
            A-Z

        NameChar:
            SpecialNameChar
            NameLetterChar

        Name:
            NameChar
            NameChar Name

        Bool:
            true
            false    

        Digit:
            0-9

        NonZeroDigit:
            1-9   
            
       Digits:
            Digit
            Digit Digits
            
        HexDigit:
            Digit
            a-f
            A-F

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

        Number:
            Integer
            Float 

        ListSeparator:
            WhiteSpace , WhiteSpace
        
        ListItems:
            ∅            
            Value 
            ListSeparator Value ListItems  
            
        List:
            OpenList ListItems CloseList

        StringAnyUnicodeChar: 
            not BackSlash or DoubleQuote
            
        StringEscapeCharValue:
             DoubleQuote
             \
             n
             r
             t
             u HexDigit HexDigit HexDigit HexDigit

        StringEscapeChar:
            \ StringEscapeCharValue

        StringChar:
            StringEscapeChar
            StringAnyUnicodeChar

        StringCharList:
            ∅
            StringChar
            StringChar StringCharList

        String:
            DoubleQuote StringCharList DoubleQuote

        Value:
            List
            Bool
            Number
            Name
            String  

        Text:
            List
    *)
    
   

    open System
    open System.Text
    open PureParse

    type PValue =
    | PList of PValue list

    | PName of string

    | PString of string

    | PNumber of double

    | PBool of bool
    
    let booleanKeywords = ["true";"false"]
    let pBool:Parser<unit,PValue> = 
        parse {
            let! t = parseKeywords booleanKeywords
            return PBool (t = "true")
        } <??> ("Parse Boolean", "Expecting true|false")

    let private digitToInt = function
        | x when x >= '0' && x <= '9' -> int x - int '0'
        | x when x >= 'a' && x <= 'f' -> 10 + int x - int 'a'
        | x when x >= 'A' && x <= 'F' -> 10 + int x - int 'A'
        | _ -> failwith ""

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

    let private parseDigits<'TState> : Parser<'TState, List<Rune>> = 
        parseList parseDigit (Many 1)

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
            let! integer = parseDecimalInteger
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

    let private pNumber:Parser<unit,PValue> = 
        (map (parseFloat <|> map parseInteger double) PNumber) 
            <??> ("Number Parser", "Parse an integer or real number.")
            
    let private pStringAnyUnicodeChar = parseNoneOf "\\\""

    let private pStringEscapeCharValue =
        parse {
            let! c = parseAnyOf (RuneCharSeq [ 'r'; 'n'; 't'; '\"'; '\\' ])
            match c with
            | Rune 'r' -> return Rune('\r')
            | Rune 'n' -> return Rune('\n')
            | Rune 't' -> return Rune('\t')
            | Rune '\"' -> return Rune('\"')
            | Rune '\\' -> return Rune('\\')
            | _ -> return! fail "Invalid escape sequence."
        }

    let private pStringEscapeChar =
        parse {
            let! _ = parseChar '\\'
            let! c = pStringEscapeCharValue
            return c
        }

    let private pStringChar = 
        pStringEscapeChar <|> pStringAnyUnicodeChar

    let private pStringCharList = 
        parseCharString pStringChar

    let private pDoubleQuote = 
        skipChar '\"'

    let private pString =
        parse {
            do! pDoubleQuote
            let! body =  pStringCharList
            do! pDoubleQuote
            return PString body
        } <??> ("String Parser", "\"...body...\"")

    let private whiteSpaceChars = RuneCharSeq [' '; '\r'; '\n'; '\t'; ]
    let private pWhiteSpace:Parser<unit,unit> = 
        parse {
            let! _ = parseCharString (parseAnyOf whiteSpaceChars)
            return ()
        }

    let pSkipAtLeastOneWhiteSpace:Parser<unit,unit> = 
        map (parseList (parseAnyOf whiteSpaceChars) (Many 1)) (fun _ -> ())

    let private specialChars = (['!';'@';'#';'$';'^';'&';'*';'-';'+';'=';'?';'/';'<';'>';'~';'`';'_';'[';']';';';':'] |> Set.ofList)
    let private nameChars = RuneCharSeq (asciiLetterChars |> Set.union specialChars)
    let private pName:Parser<unit,PValue> = map (parseCharString (parseAnyOf nameChars)) PName

    let private pOpenList:Parser<unit, unit> =
        parse {
            do! skip pWhiteSpace
            do! skipChar '('
            do! skip pWhiteSpace
            return ()
        }

    let private pCloseList:Parser<unit, unit> =
        parse {
            do! skip pWhiteSpace
            do! skipChar ')'
            do! skip pWhiteSpace
            return ()
        }   
        
    let private pList:Parser<unit,PValue> = 
        parse {
            do! pOpenList
            let! values = parseList pValue (Sep (pSkipAtLeastOneWhiteSpace, true, 0)) 
            do! pCloseList
            return PList values
        }

    let private pValue = chooseSync [  pNumber; pBool <|> pName; pString; pList; ] 

    let parseText text = run pList text ()