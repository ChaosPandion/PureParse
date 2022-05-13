﻿namespace rec PureParse.Examples

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

    let private skipWhiteSpace:Parser<unit,unit> = skip (parseMany parseOneSpace)
    
    let private hexDigits = RuneCharSeq (['0'..'9'] @ ['a'..'f'] @ ['A'..'F'])

    let private parseOneHexDigit = parseAnyOf hexDigits

    let private hexDigit:Parser<unit,char> = 
        map parseOneHexDigit (fun r -> char r.Value)
        
    let private oneNineDigit = RuneCharSeq (['1'..'9'])

    let private oneNine:Parser<unit,char> = 
        map (parseAnyOf oneNineDigit) (fun r -> char r.Value) 

    let private digit:Parser<unit,char> = parseChar '0' <|> oneNine

    let private digits:Parser<unit,char list> = parseMany1 digit

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
 
    let private parseIntegerPart =
        parse {
            let! sign = sign
            let! digits = digits
            let signModifier = parseSign sign
            let integerPart = parseInteger digits
            return integerPart, signModifier
        }

    let private exponentChar = RuneCharSeq "eE"

    let private parseExponentPart =
        parse {
            let! _ = parseAnyOf exponentChar
            let! s = sign
            let! d = digits
            let signModifier = parseSign s;
            let integerPart = parseInteger d;
            let power = signModifier * integerPart
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
                let! h1 = hexDigit
                let! h2 = hexDigit 
                let! h3 = hexDigit 
                let! h4 = hexDigit
                let v = ((digitToInt h1) * 4096) + 
                            ((digitToInt h2) * 256) + 
                            ((digitToInt h3) * 16) + 
                            (digitToInt h4)
                return Rune(char v)
            | _ -> return! fail "fatal error"
        }

    let private stringChars = parseMany (nonEscapeChar <|> escapeChar)

    let private pStringBody:Parser<unit,string> = 
        parse {
            let! chars = stringChars
            return System.String.Join("", chars)
        }      
        
    let private pString:Parser<unit,string> = 
        parse {
            do! skipChar '\"'
            let! body = pStringBody
            do! skipChar '\"'
            return body
        }

    let private parseJsonNumber =
        parse {
            let! ip, sign = parseIntegerPart  
            let! fp = parseFractionalPart <|> result 0.0
            let! ep = parseExponentPart <|> result 1.0
            let x = ip + fp
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

    let private pKeyword:Parser<unit,JsonValue> = 
        parse {
            match! parseKeywords keywords with
            | "true" -> return JsonBoolean true
            | "false" -> return JsonBoolean false
            | "null" -> return JsonNull
            | _ -> return! fail "No Valid Keyword"
        }

    let private parseMember:Parser<unit,string * JsonValue> = 
        parse {
            do! skipWhiteSpace
            let! name = pString
            do! skipWhiteSpace
            do! skipChar ':'
            do! skipWhiteSpace
            let! value = pValue
            do! skipWhiteSpace
            return name, value
        }

    let private parseMembers:Parser<unit,Map<string, JsonValue>> = 
        parse {
            let! members = parseList parseMember (skipWhiteSpace  |-> skipChar ',' |-> skipWhiteSpace) false
            return members |> Map.ofSeq
        }         

    let private pElement:Parser<unit,JsonValue> = 
        parse {
            do! skipWhiteSpace
            let! value = pValue
            do! skipWhiteSpace
            return value
        }

    let private pElements:Parser<unit,List<JsonValue>> = 
        parse {
            return! parseList pElement (skipChar ',') false
        }  

    let private pObject:Parser<unit,JsonValue> = 
        parse {
            do! skipWhiteSpace
            do! skipChar '{'
            do! skipWhiteSpace
            let! members = opt parseMembers
            do! skipWhiteSpace
            do! skipChar '}'
            do! skipWhiteSpace
            return JsonObject (Option.defaultWith (fun () -> Map.empty) members)
        }

    let private pArray:Parser<unit,JsonValue> = 
        parse {
            do! skipWhiteSpace
            do! skipChar '['
            do! skipWhiteSpace
            let! elements = opt pElements
            do! skipWhiteSpace
            do! skipChar ']'
            do! skipWhiteSpace
            return JsonArray (Option.defaultWith (fun () -> List.empty) elements)
        }

    let private pValue:Parser<unit,JsonValue> = 
        parse {
            do! skipWhiteSpace
            let! v = choose [ parseJsonString; pKeyword; parseJsonNumber; pObject; pArray  ]
            do! skipWhiteSpace
            return v
        }

    let parseText text = run pValue text ()