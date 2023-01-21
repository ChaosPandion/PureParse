namespace PureParse

open System
open System.Text
open System.Numerics
open System.Globalization

[<AutoOpen>]
module TextParsers =
    
    [<Literal>]
    let private DoesNotSupportSurrogates = "This function does not support surrogates."

    [<Literal>]
    let private RangeParserName = "Character Range"

    let inline private lowerGreaterThanUpper x y = 
        failwithf "The value %O must be less than %O" x y

    let inline private rangeErrorMessage x y = 
        $"Expect Range: '{x}' - '{y}'"                
    
    let private toStringMessage (x:obj) : string =
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

    /// Parse a single char within the range of characters specified.
    let charRange<'TState> (lower:char) (upper:char) : Parser<'TState, char> =
        if lower >= upper then 
            lowerGreaterThanUpper lower upper
        if Char.IsSurrogate(lower) || Char.IsSurrogate(upper) then 
            failwith DoesNotSupportSurrogates
        let message = rangeErrorMessage lower upper
        let lower = Rune(lower)
        let upper = Rune(upper)
        fun (stream:TextStream<'TState>) -> 
            match stream.Next() with
            | ValueSome(r, ns) 
                when r >= lower && r <= upper -> 
                    Success(ns, toChar r)
            | _ -> 
                let data = stream.CreateErrorEventData(RangeParserName, message)
                stream.ReportEvent(ParseFailure(data))
                Failure(stream)

    /// Parse a single rune within the range of runes specified.
    let runeRange<'TState> (lower:Rune) (upper:Rune) : Parser<'TState, Rune> =
        if lower >= upper then 
            lowerGreaterThanUpper lower upper
        let message = rangeErrorMessage lower upper
        fun (stream:TextStream<'TState>) -> 
            match stream.Next() with
            | ValueSome(r, ns) 
                when r >= lower && r <= upper -> 
                    Success(ns, r)
            | _ ->
                let data = stream.CreateErrorEventData(RangeParserName, message)
                stream.ReportEvent(ParseFailure(data))
                Failure(stream)

    /// Parse a single rune from the stream.
    let parseRune<'TState> (r:Rune) : Parser<'TState, Rune> =
        fun (stream:TextStream<'TState>) -> 
            match stream.Next() with
            | ValueSome(x, ns) when x = r -> Success(ns, r)
            | _ ->
                stream.ReportEvent(ParseFailure(stream.CreateErrorEventData("Character", $"Expecting --> {r}")))
                Failure(stream)

    /// Parse a single non-surrogate character.
    let parseChar<'TState> (c:char) : Parser<'TState, char> =
        if Char.IsSurrogate(c) then
            failwith DoesNotSupportSurrogates
        let expect = Rune(c)
        fun (stream:TextStream<'TState>) -> 
            match stream.Next() with
            | ValueSome(r, ns) when r = expect -> Success(ns, c)
            | _ ->
                stream.ReportEvent(ParseFailure(stream.CreateErrorEventData("Character", $"Expecting --> {expect}")))
                Failure(stream)

    /// Parse a non-empty string of characters.
    let parseString<'TState> (s:string) : Parser<'TState, string> =  
        if String.IsNullOrEmpty s then
            invalidArg (nameof s) "Must be a non-empty string."
        fun (stream:TextStream<'TState>) ->     
            match stream.Next(s) with
            | ValueSome(_, ns) -> Success(ns, s)
            | _ ->
                stream.ReportEvent(ParseFailure(stream.CreateErrorEventData("String", $"Expecting --> '{s}'")))
                Failure(stream)

    /// Parse one or more white space characters returning a string.
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

    /// Parse the provided rune replacing the result with unit.
    let skipRune<'TState> (r:Rune) : Parser<'TState, unit> =
        let message = $"Expecting >>> {toStringMessage r}"
        fun (stream:TextStream<'TState>) -> 
            match stream.Next() with
            | ValueSome(x, ns) when x = r -> Success(ns, ())
            | _ ->
                stream.ReportEvent(ParseFailure(stream.CreateErrorEventData("String",  message)))
                Failure(stream)

    /// Parse the provided character replacing the result with unit.
    let skipChar<'TState> (c:char) : Parser<'TState, unit> =
        let expect = Rune(c)
        let message = $"Expecting --> {toStringMessage expect}"
        fun (stream:TextStream<'TState>) -> 
            match stream.Next() with
            | ValueSome(r, ns) when r = expect -> Success(ns, ())
            | _ ->
                stream.ReportEvent(ParseFailure(stream.CreateErrorEventData("String", message)))
                Failure(stream)

    /// Skips passed the provided non-empty string of characters.
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
        
    /// Skips past zero or more unicode white space characters,
    let skipWhiteSpace<'TState> () : Parser<'TState, unit> =  
        fun (stream:TextStream<'TState>) ->   
            match stream.Next(Rune.IsWhiteSpace) with
            | ValueSome (_, stream) -> Success (stream, ()) 
            | ValueNone -> Success (stream, ()) 

    /// Parse one rune from within the set provided. When contains is true the set must contain the rune found,
    /// otherwise the set must not contain the rune found.
    let parseRuneBySet<'TState> (set:Set<Rune>) (contains:bool) : Parser<'TState, Rune> =
        let message = $"""Expecting >>> {System.String.Join(",", set |> Set.map toStringMessage)}."""
        let parserName = "Rune Set"
        fun (stream:TextStream<'TState>) -> 
            match stream.Next() with
            | ValueSome(r, ns) when contains && set.Contains r -> Success(ns, r)
            | ValueSome(r, ns) when not contains && not <| set.Contains r -> Success(ns, r)
            | _ ->
                stream.ReportEvent(ParseFailure(stream.CreateErrorEventData(parserName, message)))
                Failure(stream)
                
    /// Parse one rune found within the provided set.
    let parseAnyRuneInSet<'TState> (set:Set<Rune>) : Parser<'TState, Rune> = 
        let message = $"""Expecting >>> {System.String.Join(",", set |> Set.map toStringMessage)}."""
        fun (stream:TextStream<'TState>) -> 
            match stream.Next() with
            | ValueSome(r, ns) when set.Contains r -> Success(ns, r)
            | _ ->
                stream.ReportEvent(ParseFailure(stream.CreateErrorEventData("Rune Set", message)))
                Failure(stream)

    /// Parse one rune not found within the provided set.
    let parseAnyRuneNotInSet<'TState> (set:Set<Rune>) : Parser<'TState, Rune> = 
        let message = $"""Expecting >>> {System.String.Join(",", set |> Set.map toStringMessage)}."""
        fun (stream:TextStream<'TState>) -> 
            match stream.Next() with
            | ValueSome(r, ns) when not <| set.Contains r -> Success(ns, r)
            | _ ->
                stream.ReportEvent(ParseFailure(stream.CreateErrorEventData("Rune Set", message)))
                Failure(stream)

    /// Parse one rune provided.
    let parseAnyOf<'TState> (value:RuneData) : Parser<'TState, Rune> =
        let set = match value with
                  | RuneSet set -> set 
                  | RuneSeq rs -> Set.ofSeq rs
                  | RuneData.RuneString s -> Set.ofSeq (s.EnumerateRunes())
                  | RuneData.RuneCharSeq cs -> Set.ofSeq (cs |> Seq.map Rune)
                  | RuneCharArray cs ->  Set.ofSeq (cs |> Seq.map Rune)
        parseAnyRuneInSet set       
        
    /// Parse any rune not found in the provided characters.
    let parseNoneOf<'TState> (value:char seq) : Parser<'TState, Rune> = 
        parseAnyRuneNotInSet (Set.ofSeq (value |> Seq.map Rune))

    /// Parse a string of characters using the provided parser.
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
                            yield y, x.Substring(0, i) }
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