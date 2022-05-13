namespace PureParse.Examples.PLisp

#nowarn "40";

module rec PLisp =

    open System
    open System.Text
    open PureParse

    type PValue =
    | PList of PValue list

    | PName of string

    | PString of string

    | PNumber of double

    | PBool of bool
    
    let pBool:Parser<unit,PValue> = 
        parse {
            let! t = parseKeywords [ "true"; "false" ]
            return PBool (t = "true")
        }

    let firstDigit = RuneCharSeq asciiDigitNoZeroChars
    let digit = RuneCharSeq asciiDigitChars
    let pInteger = 
        parse {
            let! x = parseAnyOf firstDigit
            let! integer = parseCharString (parseAnyOf digit)
            return double (x.ToString() + integer)
        }

    let pFractional = 
        parse {
            let! fractional = skipChar '.' |-> parseCharString (parseAnyOf digit)
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
        } <??> ("Number Parser", "Parse an integer or real number.")

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
        } <??> ("String Parser", "\"...body...\"")

    let whiteSpaceChars = RuneCharSeq [' '; '\r'; '\n'; '\t'; ]
    let pWhiteSpace:Parser<unit,unit> = 
        parse {
            let! _ = parseCharString (parseAnyOf whiteSpaceChars)
            return ()
        }

    let private nameChars = RuneCharSeq (asciiLetterChars)
    let private pName:Parser<unit,PValue> = map (parseCharString (parseAnyOf nameChars)) PName
        
    let pList:Parser<unit,PValue> = 
        parse {
            do! skip pWhiteSpace
            do! skipChar '('
            do! skip pWhiteSpace
            let! values = parseList pValue (Sep (pWhiteSpace, true, 0)) 
            do! skip pWhiteSpace
            do! skipChar ')'
            do! skip pWhiteSpace
            return PList values
        }

    let pValue = choose [ pBool <|> pName; pString; pNumber; pList; ] 

    let parseText text = run pList text ()