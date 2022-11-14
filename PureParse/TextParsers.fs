namespace PureParse

open System
open System.Text

[<AutoOpen>]
module TextParsers =

    let charRange<'TState> (lower:char) (upper:char) : Parser<'TState, char> =
        if lower >= upper then
            failwithf "The value %c must be less than %c" lower upper
        if Char.IsSurrogate(lower) || Char.IsSurrogate(upper) then
            failwith "This function does not support surrogates."
        let message = sprintf "Expect Range: '%c' - '%c'" lower upper
        let lower = Rune(lower)
        let upper = Rune(upper)
        fun (stream:TextStream<'TState>) -> 
            match stream.Next() with
            | ValueSome(r, ns) when r >= lower && r <= upper -> Success(ns, toChar r)
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
        fun (stream:TextStream<'TState>) ->     
            match stream.Next(s) with
            | ValueSome(_, ns) -> Success(ns, s)
            | _ ->
                stream.ReportEvent(ParseFailure(stream.CreateErrorEventData("String", $"Expecting --> '{s}'")))
                Failure(stream)

    let parseWhiteSpace<'TState> () : Parser<'TState, string> =  
        fun (stream:TextStream<'TState>) ->
            match stream.Next (Rune.IsWhiteSpace) with
            | ValueSome (value, stream) -> 
                let append (sb:StringBuilder) (r:Rune) = sb.Append (r.ToString()) 
                let sb = value.ToArray() |> Array.fold append (StringBuilder())
                Success(stream, sb.ToString())
            | ValueNone ->
                stream.ReportEvent(ParseFailure(stream.CreateErrorEventData("String", $"Expecting --> white space.")))
                Failure(stream)                 
    
    let toStringMessage (x:obj) : string =
        let charMap c  =
            match c with
            | c when Char.IsWhiteSpace(c) -> "\\u" + Rune(c).Value.ToString("X4")
            | c -> "'" + c.ToString() + "'"
        let fail m = 
            failwith $"""The type {if x = null then "(null)" else x.GetType().ToString()} cannot be converted to a string message.\n{m}"""
        match x with
        | null -> fail "The input was null."
        | :? string as s when s.Length = 1 -> charMap s[0]
        | :? string as s -> "'" + (s |> Seq.map charMap |> Seq.reduce (+)) + "'"         
        | :? char as c -> charMap c
        | :? Rune as r when Rune.IsWhiteSpace(r) -> "\\u" + r.Value.ToString("X4")
        | :? Rune as r -> "'" + r.ToString() + "'"
        | _ -> fail "The type is not a variant of char."

    let skipRune<'TState> (r:Rune) : Parser<'TState, unit> =
        fun (stream:TextStream<'TState>) -> 
            match stream.Next() with
            | ValueSome(x, ns) when x = r -> Success(ns, ())
            | _ ->
                stream.ReportEvent(ParseFailure(stream.CreateErrorEventData("String",  $"Expecting --> {toStringMessage r}")))
                Failure(stream)
    let skipChar<'TState> (c:char) : Parser<'TState, unit> =
        let expect = Rune(c)
        fun (stream:TextStream<'TState>) -> 
            match stream.Next() with
            | ValueSome(r, ns) when r = expect -> Success(ns, ())
            | _ ->
                stream.ReportEvent(ParseFailure(stream.CreateErrorEventData("String",  $"Expecting --> {toStringMessage expect}")))
                Failure(stream)
    let skipString<'TState> (s:string) : Parser<'TState, unit> =  
        if String.IsNullOrEmpty s then
            invalidArg (nameof s) "Must be a non-empty string."
        let count = s.Length
        fun (stream:TextStream<'TState>) ->   
            match stream.Next(count) with
            | ValueSome(Runes s, ns) -> Success(ns, ())
            | _ ->
                stream.ReportEvent(ParseFailure(stream.CreateErrorEventData("String", $"Expecting --> {toStringMessage s}")))
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
            else Success (stream, ()) 
                //stream.ReportEvent(ParseFailure(stream.CreateErrorEventData("String", $"Expecting --> white space.")))
                //Failure(stream)


    let parseRuneBySet<'TState> (set:Set<Rune>) (contains:bool) : Parser<'TState, Rune> =
        let message = $"""Expecting --> {System.String.Join(",", set |> Set.map toStringMessage)}.""" 
        let parserName = "Rune Set"
        fun (stream:TextStream<'TState>) -> 
            match stream.Next() with
            | ValueSome(r, ns) when contains && set.Contains r -> Success(ns, r)
            | ValueSome(r, ns) when not contains && not <| set.Contains r -> Success(ns, r)
            | _ ->
                stream.ReportEvent(ParseFailure(stream.CreateErrorEventData(parserName, message)))
                Failure(stream)

    //let parseAnyRuneInSet<'TState> (set:Set<Rune>) : Parser<'TState, Rune> = parseRuneBySet set true

    //let parseAnyRuneNotInSet<'TState> (set:Set<Rune>) : Parser<'TState, Rune> = parseRuneBySet set false

    let parseAnyRuneInSet<'TState> (set:Set<Rune>) : Parser<'TState, Rune> = 
        fun (stream:TextStream<'TState>) -> 
            match stream.Next() with
            | ValueSome(r, ns) when set.Contains r -> Success(ns, r)
            | _ ->
                let message = $"""Expecting --> {System.String.Join(",", set |> Set.map toStringMessage)}."""
                stream.ReportEvent(
                    ParseFailure(stream.CreateErrorEventData("Rune Set", message)))
                Failure(stream)
    let parseAnyRuneNotInSet<'TState> (set:Set<Rune>) : Parser<'TState, Rune> = 
        fun (stream:TextStream<'TState>) -> 
            match stream.Next() with
            | ValueSome(r, ns) when not <| set.Contains r -> Success(ns, r)
            | _ ->
                let message = $"""Expecting --> {System.String.Join(",", set |> Set.map toStringMessage)}."""
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

    /// Parse the longest word within the provided list.
    let parseAnyString<'TState> (words:string list) : Parser<'TState, string> =
        if words.IsEmpty then
            failwith "No words provided."
        if words |> Seq.exists (String.IsNullOrEmpty) then
            failwith "None of the words can be null or empty."
        if (words |> Seq.distinct |> Seq.length) < (words |> List.length) then
            failwith $"""The provided list of words contains duplicates: {String.Join (",", words)}"""
        let prefixGroups = 
            seq {
                for x in words do
                    for y in words do
                        let mutable i = 0
                        while i < x.Length && i < y.Length && x[i] = y[i] do 
                            i <- i + 1
                        if i > 0 then
                            yield y, x.Substring(0, i)}
            |> Seq.groupBy (fun (_, prefix) -> prefix)
            |> Seq.map (fun (prefix, pairs) -> 
                            prefix, pairs |> Seq.sortByDescending (fun (word, _) -> word.Length) |> Seq.toList)
            |> Seq.filter (fun (_, pairs) -> pairs.Length > 1)
            |> Seq.map (fun (prefix, pairs) ->  // Take the word from the pair
                            prefix, pairs |> Seq.distinct |> Seq.map fst |> List.ofSeq)
            |> Seq.sortByDescending (fun (prefix, _) -> prefix.Length)
            |> List.ofSeq
        let wordsWithPrefix = prefixGroups |> Seq.collect (fun (_, words) -> words)
        let wordsWithoutPrefix = words |> Seq.except wordsWithPrefix
        let test (prefix:string) (keywordsWithPrefix:string list) =
            fun (stream:TextStream<_>) -> 
                match stream.Peek(prefix) with
                | ValueSome (_) ->
                    keywordsWithPrefix 
                    |> Seq.map (
                        fun s -> 
                            match stream.Next(s) with 
                            | ValueSome (_, stream) -> Success (stream, s) 
                            | _ -> Failure (stream))
                    |> Seq.filter (
                        fun x -> 
                            match x with 
                            | Success (_, _) -> true 
                            | _ -> false)
                    |> Seq.tryHead
                    |> Option.defaultValue (Failure(stream))
                | ValueNone -> 
                    Failure (stream)
        let info = ("words", $"""Expected one of the following: {String.Join (",", words)}""")
        let nonPrefixParser = choose (wordsWithoutPrefix |> Seq.map parseString<'TState> |> Seq.toList)
        if prefixGroups.IsEmpty then
            nonPrefixParser <??> info
        else
            let prefixParser = prefixGroups |> Seq.map (fun (x, y) -> test x y) |> Seq.reduce (<|>)
            (prefixParser <|> nonPrefixParser) <??> info

    let parseInt32<'TState> () : Parser<'TState, int> =
        let digitToInt = function
            | x when x >= '0' && x <= '9' -> int x - int '0'
            | x when x >= 'a' && x <= 'f' -> 10 + int x - int 'a'
            | x when x >= 'A' && x <= 'F' -> 10 + int x - int 'A'
            | x -> failwith $"The char '{x}' is not expected."
        let rec parseInteger = function
            | [] -> 0.0
            | c::cs ->
                let d = digitToInt c |> double
                let e = (10.0 ** double cs.Length)
                d * e + parseInteger cs
        fun (stream:TextStream<'TState>) ->
            match stream.Next(Rune.IsDigit) with
            | ValueNone -> 
                stream.ReportEvent(ParseFailure(stream.CreateErrorEventData("Int32", $"No Characters Remaining")))
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


            //match stream.Peek (stream.Remaining) with
            //| ValueNone -> 
            //    stream.ReportEvent(ParseFailure(stream.CreateErrorEventData("Int32", $"No Characters Remaining")))
            //    Failure (stream)
            //| ValueSome (rm:ReadOnlyMemory<Rune>) -> 
            //    let s = rm.Span
            //    let mutable e = s.GetEnumerator()
            //    let mutable count = 0
            //    let mutable complete = false
            //    while not complete do
            //        let mutable next = e.MoveNext()
            //        if next then
            //            if isDigit e.Current then
            //                count <- count + 1
            //            else 
            //                complete <- true
            //        else 
            //            complete <- true
            //    if count = 0 then
            //        stream.ReportEvent(ParseFailure(stream.CreateErrorEventData("Int32", $"No Digits Found")))
            //        Failure (stream)
            //    else 
            //        match stream.Next count with
            //        | ValueNone ->
            //            stream.ReportEvent(ParseFailure(stream.CreateErrorEventData("Int32", $"Failed to read {count} digits.")))
            //            Failure (stream)
            //        | ValueSome (rm:ReadOnlyMemory<Rune>, stream) ->
            //            let s = rm.Span
            //            let mutable i = 0
            //            for x = 0 to s.Length - 1 do
            //                let c = char (s[x].Value)
            //                let h = (digitToInt c) |> double
            //                let remaining = (s.Length - (x + 1)) |> double
            //                i <- i + int(h * (10.0 ** remaining))
            //            Success (stream, i)