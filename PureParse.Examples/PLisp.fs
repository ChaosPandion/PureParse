namespace PureParse.Examples.PLisp

#nowarn "40";

module rec PLisp =

    open System
    open System.Text
    open PureParse
    open PureParse.Parse
    open PureParse.Parsers
    open PureParse.TextParsers
    open PureParse.TextStream
    open PureParse.Runes

    type PValue =
    | PList of PValue list

    | PName of string

    | PString of string

    | PNumber of double

    | PBool of bool
    
    let pBool:Parser<unit,PValue> = 
        parse {
            let! t = parseString "true" <|> parseString "false"
            return PBool (t = "true")
        }

    let pInteger = 
        parse {
            let! integer = parseCharString (parseAnyOf (RuneString "0123456789"))
            return double integer
        }

    let pFractional = 
        parse {
            do! skipChar '.'
            let! fractional = parseCharString (parseAnyOf (RuneString "0123456789"))
            return double ("." + fractional)
        }

    let pNumber:Parser<unit,PValue> = 
        parse {
            let! integer = pInteger
            match! opt pFractional with
            | Some fractional -> 
                return PNumber (integer + fractional)
            | None -> 
                return PNumber integer
        }

    let pEscapeChar = 
        parse {
            let! _ = parseChar '\\'
            let! c = parseAnyOf (RuneCharSeq [ 'r'; 'n'; 't'; '\"'; '\\' ])
            match c with
            | Rune 'r' -> return Rune('\r')
            | Rune 'n' -> return Rune('\n')
            | Rune 't' -> return Rune('\t')
            | Rune '\"' -> return Rune('\"')
            | Rune '\\' -> return Rune('\\')
            | _ -> return! fail "Invalid escape sequence."
        }

    let pChar = parse {
        let! r = parseNoneOf "\\\""
        return r
    }

    let pStringBody = pEscapeChar <|> pChar

    let pString:Parser<unit,PValue> = 
        parse {
            do! skipChar '\"'
            let! body =  parseCharString pStringBody
            do! skipChar '\"'
            return PString body
        }

    let pWhiteSpace:Parser<unit,unit> = 
        parse {
            let! _ = parseCharString (parseAnyOf (RuneCharSeq [' '; '\r'; '\n'; '\t'; ]))
            return ()
        }

    let private nameChars = RuneString "abcdefghijklmnopqrztuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
    let private pName:Parser<unit,PValue> = map (parseCharString (parseAnyOf nameChars)) PName
        
    let pList:Parser<unit,PValue> = 
        parse {
            do! skip pWhiteSpace
            do! skipChar '('
            do! skip pWhiteSpace
            let! values = parseList pValue pWhiteSpace true 
            do! skip pWhiteSpace
            do! skipChar ')'
            do! skip pWhiteSpace
            return PList values
        }

    let pValue = choose [ pBool <|> pName; pString; pNumber; pList; ] 

    let parseText text = run pList text ()