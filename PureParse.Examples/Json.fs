namespace rec PureParse.Examples

open System.Text
open PureParse

#nowarn "40"

module Json =

    type JsonValue =
        | JsonObject of Map<string, JsonValue>
        | JsonArray of List<JsonValue>
        | JsonString of string
        | JsonNumber of double
        | JsonBoolean of bool
        | JsonNull

    let private ws = RuneCharSeq [ ' '; '\n'; '\r'; '\t'; ]

    let private parseOneSpace = parseAnyOf ws

    let private skipWhiteSpace:Parser<unit,unit> = skip (parseMany parseOneSpace 0)
    
    let private hexDigits = RuneCharSeq (['0'..'9'] @ ['a'..'f'] @ ['A'..'F'])

    let private parseOneHexDigit = parseAnyOf hexDigits

    let private hexDigit:Parser<unit,char> = 
        map parseOneHexDigit (fun r -> char r.Value)
        
    let private oneNineDigit = RuneCharSeq (['1'..'9'])

    let private oneNine:Parser<unit,char> = 
        map (parseAnyOf oneNineDigit) (fun r -> char r.Value) 

    let private digit:Parser<unit,char> = parseChar '0' <|> oneNine

    let private digits:Parser<unit,char list> = parseMany digit 1

    let private digitToInt = function
    | x when x >= '0' && x <= '9' -> int x - int '0'
    | x when x >= 'a' && x <= 'f' -> 10 + int x - int 'a'
    | x when x >= 'A' && x <= 'F' -> 10 + int x - int 'A'
    | _ -> failwith ""

    let rec private parseInteger = function
        | [] -> 0.0
        | c::cs -> 
            (digitToInt c |> double) * (10.0 ** double cs.Length) + parseInteger cs

    let rec private parseFractional = function
        | [] -> 0.0
        | c::cs -> 
            ((digitToInt c |> double) / (10.0 ** double (cs.Length - 1))) + parseFractional cs

    let rec private parseSign = function
        | Some '-' -> -1.0
        | Some '+' | None -> 1.0
        | Some _ -> 
            failwith "Unexpected Sign"

    let private sign:Parser<unit,char option> = parse { return! opt (parseChar '+' <|> parseChar '-') }
 
    let private parseOptionalMinus:Parser<unit, float> = 
        parse {
            match! opt (parseChar '-') with
            | Some '-' -> return -1.0
            | _ -> return 1.0
        }

    let private parseIntegerPart:Parser<unit, int32 * float> =
        parse {
            let! sign  = parseOptionalMinus
            let! digits = parseInt32
            return digits, sign
        }

    let private exponentChar = RuneCharSeq "eE"

    let private parseExponentPart:Parser<unit, float> =
        parse {
            let! _ = parseAnyOf exponentChar
            let! s = sign
            let! d = parseInt32
            let signModifier = parseSign s;
            let power = signModifier * (double d)
            let result = 10.0 ** power
            return result
        }

    let private parseFractionalPart =
        parse {
            do! skipChar '.'
            let! d = digits
            let n = parseInteger d;
            let pow = -(d.Length);
            let result = n * (10.0 ** pow)
            return result
        }

    let private nonEscapeChar = parseNoneOf "\"\\"

    let private escapeChars = RuneString "\"\\/bfnrtu"

    let private hexEscapeDigits : Parser<unit, char> =
        parseProduction "Hex Escape Digits" <| (parse {
            let! h1, h2, h3, h4 = sequence4 hexDigit hexDigit hexDigit hexDigit
            let c = char (((digitToInt h1) * 4096) + ((digitToInt h2) * 256) + ((digitToInt h3) * 16) + (digitToInt h4))
            return c
        } |> onFailure "A unicode escape sequence requires 4 hex digits following the \\u")
    
    let private escapeChar = 
        parse {
            do! skipChar '\\'
            let! c = parseAnyOf escapeChars
            match c with
            | RuneChar '\"' -> return Rune('\"')
            | RuneChar '\\' -> return Rune('\\')
            | RuneChar '/' -> return Rune('/')
            | RuneChar 'b' -> return Rune('\b')
            | RuneChar 'f' -> return Rune('\f')
            | RuneChar 'n' -> return Rune('\n')
            | RuneChar 'r' -> return Rune('\r')
            | RuneChar 't' -> return Rune('\t')
            | RuneChar 'u' ->
                let! c = hexEscapeDigits
                if System.Char.IsHighSurrogate c then
                    do! skipChar '\\'
                    do! skipChar 'u'
                    let! low = hexEscapeDigits
                    return Rune(c, low)
                else
                    return Rune(c)
            | _ -> return! fail "fatal error"
        }

    let private stringChars = parseList (nonEscapeChar <|> escapeChar) (Many 0)

    let private pStringBody:Parser<unit,string> = 
       parseProduction "String Body"
            <| parse {
                let! chars = stringChars
                return System.String.Join("", chars)
            }      
        
    let private pBeginString:Parser<unit,unit> = parseProduction "Begin String" (skipChar '\"')
    let private pEndString:Parser<unit,unit> = parseProduction "End String" (skipChar '\"')
    let private pString:Parser<unit,string> = 
        parseProduction "String" 
            <| parse {
            do! pBeginString
            let! body = pStringBody
            do! pEndString
            return body
        }

    let private parseJsonNumber =
        parseProduction "Number" 
            <| parse {
            let! ip, sign = parseIntegerPart  
            let! fp = parseFractionalPart <|> result 0.0
            let! ep = parseExponentPart <|> result 1.0
            let x = double ip + fp
            let signed = sign * x
            let n = signed * ep
            return JsonNumber n
        }

    let private parseJsonString:Parser<unit,JsonValue> = 
        parse {
            let! body = pString
            return JsonString body
        }

    let private keywords = [ "true"; "false"; "null" ]


    let private pNull:Parser<unit,JsonValue> =
        parseProduction "Null" 
            <| parse {
                do! skipString "null"
                return JsonNull
            }
    let private pBoolean:Parser<unit,JsonValue> =
        parseProduction "Boolean" 
            <| ((parse {
                do! skipString "true"
                return JsonBoolean true
            }) <|> (parse {
                do! skipString "false"
                return JsonBoolean false
            }))
    
    let private pMemberNameSeparator = 
        parseProduction "MemberNameSeparator" (skipWhiteSpace  |-> skipChar ':' |-> skipWhiteSpace)
    let private pMemberName:Parser<unit,string> = 
        parseProduction "Member Name" <| parse {
            do! skipWhiteSpace
            let! name = pString
            do! skipWhiteSpace
            return name
        } 
    let private parseMember:Parser<unit,string * JsonValue> = 
        parseProduction "Member" 
            <| parse {
            let! name = pMemberName
            do! pMemberNameSeparator
            let! value = pValue
            return name, value
        }
    let private pMemberSep:Parser<unit, unit> = 
        parseProduction "Member Separator" (skipWhiteSpace  |-> skipChar ',' |-> skipWhiteSpace)
    let private pMembers:Parser<unit,Map<string, JsonValue>> = 
        parseProduction "Members" 
            <| parse {
            let! members = parseList parseMember (Sep (pMemberSep, false, 0))
            return members |> Map.ofSeq
        }        
    let private pBeginObject:Parser<unit, unit> =
        parseProduction "Begin Object" (skipWhiteSpace  |-> skipChar '{' |-> skipWhiteSpace)            
    let private pEndObject:Parser<unit, unit> =
        parseProduction "End Object" (skipWhiteSpace  |-> skipChar '}' |-> skipWhiteSpace)
    let private pObject:Parser<unit,JsonValue> = 
        parseProduction "Object" 
            <| parse {
            do! pBeginObject
            let! members = pMembers
            do! pEndObject
            return JsonObject members
        }           

    let private pElement:Parser<unit,JsonValue> = 
        parseProduction "Element" <| parse {
            do! skipWhiteSpace
            let! value = pValue
            do! skipWhiteSpace
            return value
        }
    let private pElementSeparator:Parser<unit, unit> = 
        parseProduction "Element Separator" (skipWhiteSpace  |-> skipChar ',' |-> skipWhiteSpace)
    let private pElements:Parser<unit,List<JsonValue>> = 
        parseProduction "Elements" (parseList pElement (Sep (pElementSeparator, false, 0)))
    let private pBeginArray:Parser<unit,unit> = 
        parseProduction "Begin Array" (skipWhiteSpace |-> skipChar '[' |-> skipWhiteSpace)
    let private pEndArray:Parser<unit,unit> = 
        parseProduction "End Array" (skipWhiteSpace |-> skipChar ']' |-> skipWhiteSpace)
    let private pArray:Parser<unit,JsonValue> = 
        parseProduction "Array" <| parse {
            do! pBeginArray
            let! elements = pElements
            do! pEndArray
            return JsonArray elements
        }

    let private pValue:Parser<unit,JsonValue> = 
        parseProduction "Value" 
            <| parse {
                do! skipWhiteSpace
                let! v = chooseSync [ 
                    parseJsonString; 
                    pBoolean;
                    pNull;
                    parseJsonNumber; 
                    pObject; 
                    pArray  ]
                do! skipWhiteSpace
                return v
            }

    let parseText text = 
        match tryRun pValue text () with
        | RunSuccess(_, value, _) -> value
        | _ -> failwith("Failed to parse.")
    let parser = pValue