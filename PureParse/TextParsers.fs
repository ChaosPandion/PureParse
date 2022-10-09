namespace PureParse

open System
open System.Text

[<AutoOpen>]
module TextParsers =

    let charRange<'TState> (lower:char) (upper:char) : Parser<'TState, Rune> =
        if lower >= upper then
            failwithf "The value %c must be less than %c" lower upper
        if Char.IsSurrogate(lower) || Char.IsSurrogate(upper) then
            failwith "This function does not support surrogates."
        let message = sprintf "Expect Range: '%c' - '%c'" lower upper
        let lower = Rune(lower)
        let upper = Rune(upper)
        fun (stream:TextStream<'TState>) -> 
            match stream.Next() with
            | ValueSome(r, ns) when r >= lower && r <= upper -> Success(ns, r)
            | _ -> 
                stream.ReportEvent(ParseFailure(stream.CreateErrorEventData("Character Range", message)))
                Failure(stream)

    let runeRange<'TState> (lower:Rune) (upper:Rune) : Parser<'TState, Rune> =
        if lower >= upper then
            failwithf "The value %O must be less than %O" lower upper
        let message = sprintf "Expect Range: '%O' - '%O'" lower upper
        fun (stream:TextStream<'TState>) -> 
            match stream.Next() with
            | ValueSome(r, ns) when r >= lower && r <= upper -> Success(ns, r)
            | _ ->
                stream.ReportEvent(ParseFailure(stream.CreateErrorEventData("Character Range", message)))
                Failure(stream)

    let parseRune<'TState> (r:Rune) : Parser<'TState, Rune> =
        fun (stream:TextStream<'TState>) -> 
            match stream.Next() with
            | ValueSome(x, ns) when x = r -> Success(ns, r)
            | _ ->
                stream.ReportEvent(ParseFailure(stream.CreateErrorEventData("Character", $"Expecting --> {r}")))
                Failure(stream)

    let parseChar<'TState> (c:char) : Parser<'TState, char> =
        let expect = Rune(c)
        fun (stream:TextStream<'TState>) -> 
            match stream.Next() with
            | ValueSome(r, ns) when r = expect -> Success(ns, c)
            | _ ->
                stream.ReportEvent(ParseFailure(stream.CreateErrorEventData("Character", $"Expecting --> {expect}")))
                Failure(stream)

    let parseString<'TState> (s:string) : Parser<'TState, string> =  
        if String.IsNullOrEmpty s then
            invalidArg (nameof s) "Must be a non-empty string."
        let count = s.Length
        fun (stream:TextStream<'TState>) ->     
            match stream.Next(count) with
            | ValueSome(Runes s, ns) -> Success(ns, s)
            | _ ->
                stream.ReportEvent(ParseFailure(stream.CreateErrorEventData("String", $"Expecting --> '{s}'")))
                Failure(stream)

    let parseWhiteSpace<'TState> () : Parser<'TState, string> =  
        fun (stream:TextStream<'TState>) ->             
            let rec loop (stream:TextStream<'TState>) (sb:System.Text.StringBuilder) =
                match stream.Next() with
                | ValueSome(r, ns) when Rune.IsWhiteSpace(r) ->
                    loop ns (sb.Append(r.ToString()))
                | _ -> stream, sb
            let nextStream, sb = loop stream (System.Text.StringBuilder())
            if nextStream.Index <> stream.Index
            then Success (nextStream, sb.ToString())
            else
                stream.ReportEvent(ParseFailure(stream.CreateErrorEventData("String", $"Expecting --> white space.")))
                Failure(stream)

    let skipRune<'TState> (r:Rune) : Parser<'TState, unit> =
        fun (stream:TextStream<'TState>) -> 
            match stream.Next() with
            | ValueSome(x, ns) when x = r -> Success(ns, ())
            | _ ->
                stream.ReportEvent(ParseFailure(stream.CreateErrorEventData("String",  $"Expecting --> {r}")))
                Failure(stream)

    let skipChar<'TState> (c:char) : Parser<'TState, unit> =
        let expect = Rune(c)
        fun (stream:TextStream<'TState>) -> 
            match stream.Next() with
            | ValueSome(r, ns) when r = expect -> Success(ns, ())
            | _ ->
                stream.ReportEvent(ParseFailure(stream.CreateErrorEventData("String",  $"Expecting --> {expect}")))
                Failure(stream)

    let skipString<'TState> (s:string) : Parser<'TState, unit> =  
        if String.IsNullOrEmpty s then
            invalidArg (nameof s) "Must be a non-empty string."
        let count = s.Length
        fun (stream:TextStream<'TState>) ->   
            match stream.Next(count) with
            | ValueSome(Runes s, ns) -> Success(ns, ())
            | _ ->
                stream.ReportEvent(ParseFailure(stream.CreateErrorEventData("String", $"Expecting --> '{s}'")))
                Failure(stream)
        
    let skipWhiteSpace<'TState> () : Parser<'TState, unit> =  
        fun (stream:TextStream<'TState>) ->   
            let rec loop (stream:TextStream<'TState>) =
                match stream.Next() with
                | ValueSome(r, ns) when Rune.IsWhiteSpace(r) -> loop ns 
                | _ -> stream
            let nextStream = loop stream 
            if nextStream.Index <> stream.Index
            then Success (nextStream, ())
            else
                stream.ReportEvent(ParseFailure(stream.CreateErrorEventData("String", $"Expecting --> white space.")))
                Failure(stream)

    let parseAnyRuneInSet<'TState> (set:Set<Rune>) : Parser<'TState, Rune> = 
        fun (stream:TextStream<'TState>) -> 
            match stream.Next() with
            | ValueSome(r, ns) when set.Contains r -> Success(ns, r)
            | _ ->
                let map s =
                    if Rune.IsWhiteSpace(s) then
                        "\\u" + s.Value.ToString("X4")
                    else
                        "'" + s.ToString() + "'"
                let message = $"""Expecting --> {System.String.Join(",", set |> Set.map map)}."""
                stream.ReportEvent(
                    ParseFailure(stream.CreateErrorEventData("Rune Set", message)))
                Failure(stream)

    let parseAnyRuneNotInSet<'TState> (set:Set<Rune>) : Parser<'TState, Rune> = 
        fun (stream:TextStream<'TState>) -> 
            match stream.Next() with
            | ValueSome(r, ns) when not <| set.Contains r -> Success(ns, r)
            | _ ->
                let map s =
                    if Rune.IsWhiteSpace(s) then
                        "\\u" + s.Value.ToString("X4")
                    else
                        "'" + s.ToString() + "'"
                let message = $"""Expecting --> {System.String.Join(",", set |> Set.map map)}."""
                stream.ReportEvent(
                    ParseFailure(stream.CreateErrorEventData("Rune Set", message)))
                Failure(stream)

    let parseAnyOf<'TState> (value:RuneData) : Parser<'TState, Rune> =
        let set = match value with
                  | RuneSet set -> set 
                  | RuneSeq rs -> Set.ofSeq rs
                  | RuneData.RuneString s -> Set.ofSeq (s.EnumerateRunes())
                  | RuneData.RuneCharSeq cs -> Set.ofSeq (cs |> Seq.map Rune)
                  | RuneCharArray cs ->  Set.ofSeq (cs |> Seq.map Rune)
        parseAnyRuneInSet set       

    let parseNoneOf<'TState> (value:char seq) : Parser<'TState, Rune> = parseAnyRuneNotInSet (Set.ofSeq (value |> Seq.map Rune))

    let parseCharString<'TState> (p:M<'TState, Rune>) : Parser<'TState, string>  =
        fun (stream:TextStream<'TState>) -> 
            let rec run stream (sb:System.Text.StringBuilder) =
                match p stream with
                | Success (stream, v) -> 
                    sb.Append(v.ToString()) |> ignore
                    run stream sb
                | Failure (stream) -> stream
            let sb = System.Text.StringBuilder()
            let nextStream = run stream sb
            if nextStream.Index <> stream.Index
            then Success (nextStream, sb.ToString())
            else
                stream.ReportEvent(ParseFailure(stream.CreateErrorEventData("String", $"Expecting provided string")))
                Failure(stream)            
              
    let parseKeywords<'TState> (keywords:string list) : Parser<'TState, string>  =
        if keywords.IsEmpty then
            failwith "No keywords provided."
        if keywords |> Seq.exists (String.IsNullOrEmpty) then
            failwith "None of the keywords can be null or empty."
        let description = String.Join (",", keywords)
        let parsers = keywords |> Seq.map parseString<'TState> |> Seq.toList
        (chooseSync parsers) <??> ("keywords", description)


    let private digitToInt = function
        | x when x >= '0' && x <= '9' -> int x - int '0'
        | x when x >= 'a' && x <= 'f' -> 10 + int x - int 'a'
        | x when x >= 'A' && x <= 'F' -> 10 + int x - int 'A'
        | _ -> failwith ""

    let rec private parseInteger = function
        | [] -> 0.0
        | c::cs -> (digitToInt c |> double) * (10.0 ** double cs.Length) + parseInteger cs

    let parseInt32<'TState> : Parser<'TState, int> = 
        fun (stream:TextStream<'TState>) -> 
            match stream.Peek (stream.Remaining) with
            | ValueNone -> 
                stream.ReportEvent(ParseFailure(stream.CreateErrorEventData("Int32", $"No Characters Remaining")))
                Failure (stream)
            | ValueSome (rm:ReadOnlyMemory<Rune>) -> 
                let s = rm.Span
                let mutable e = s.GetEnumerator()
                let mutable count = 0
                let mutable complete = false
                while not complete do
                    let mutable next = e.MoveNext()
                    if next then
                        if isDigit e.Current then
                            count <- count + 1
                        else 
                            complete <- true
                    else 
                        complete <- true
                if count = 0 then
                    stream.ReportEvent(ParseFailure(stream.CreateErrorEventData("Int32", $"No Digits Found")))
                    Failure (stream)
                else 
                    match stream.Next count with
                    | ValueNone ->
                        stream.ReportEvent(ParseFailure(stream.CreateErrorEventData("Int32", $"Failed to read {count} digits.")))
                        Failure (stream)
                    | ValueSome (rm:ReadOnlyMemory<Rune>, stream) ->
                        let s = rm.Span
                        let mutable i = 0
                        for x = 0 to s.Length - 1 do
                            let c = char (s[x].Value)
                            let h = (digitToInt c) |> double
                            let remaining = (s.Length - (x + 1)) |> double
                            i <- i + int(h * (10.0 ** remaining))
                        Success (stream, i)