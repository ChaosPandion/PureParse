namespace PureParse

open System
open System.Text
open FSharp.NativeInterop
open System.Numerics;
open System.Runtime.Intrinsics;
open Runes


open TextStream

module TextParsers =

    let parseRune<'TState> (r:Rune) : Parser<'TState, Rune> =
        fun (stream:TextStream<'TState>) -> 
            match stream.Next() with
            | ValueSome(x, ns) when x = r -> Success(ns, r)
            | _ -> Failure(stream, RuneParseError(r, stream.Line, stream.Column))

    let parseChar<'TState> (c:char) : Parser<'TState, char> =
        fun (stream:TextStream<'TState>) -> 
            match stream.Next() with
            | ValueSome(Rune c, ns) -> Success(ns, c)
            | _ -> Failure(stream, CharParseError(c, stream.Line, stream.Column))

    let parseString<'TState> (s:string) : Parser<'TState, string> =  
        if String.IsNullOrEmpty s then
            invalidArg (nameof s) "Must be a non-empty string."
        let count = s.Length
        fun (stream:TextStream<'TState>) ->     
            match stream.Next(count) with
            | ValueSome(Runes s, ns) -> Success(ns, s)
            | _ -> Failure(stream, StringParseError(s, stream.Line, stream.Column))

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
            else Failure (stream, NoWhiteSpaceParseError(stream.Line, stream.Column))

    let skipRune<'TState> (r:Rune) : Parser<'TState, unit> =
        fun (stream:TextStream<'TState>) -> 
            match stream.Next() with
            | ValueSome(x, ns) when x = r -> Success(ns, ())
            | _ -> Failure(stream, RuneParseError(r, stream.Line, stream.Column))

    let skipChar<'TState> (c:char) : Parser<'TState, unit> =
        fun (stream:TextStream<'TState>) -> 
            match stream.Next() with
            | ValueSome(Rune c, ns) -> Success(ns, ())
            | _ -> Failure(stream, CharParseError(c, stream.Line, stream.Column))

    let skipString<'TState> (s:string) : Parser<'TState, unit> =  
        if String.IsNullOrEmpty s then
            invalidArg (nameof s) "Must be a non-empty string."
        let count = s.Length
        fun (stream:TextStream<'TState>) ->   
            match stream.Next(count) with
            | ValueSome(Runes s, ns) -> Success(ns, ())
            | _ -> Failure(stream, StringParseError(s, stream.Line, stream.Column))
        
    let skipWhiteSpace<'TState> () : Parser<'TState, unit> =  
        fun (stream:TextStream<'TState>) ->   
            let rec loop (stream:TextStream<'TState>) =
                match stream.Next() with
                | ValueSome(r, ns) when Rune.IsWhiteSpace(r) -> loop ns 
                | _ -> stream
            let nextStream = loop stream 
            if nextStream.Index <> stream.Index
            then Success (nextStream, ())
            else Failure (stream, NoWhiteSpaceParseError(stream.Line, stream.Column))

    let parseAnyRuneInSet<'TState> (set:Set<Rune>) : Parser<'TState, Rune> = 
        fun (stream:TextStream<'TState>) -> 
            match stream.Next() with
            | ValueSome(r, ns) when set.Contains r -> Success(ns, r)
            | _ -> Failure (stream, ParseError("No matching rune was found.", stream.Line, stream.Column))

    let parseAnyRuneNotInSet<'TState> (set:Set<Rune>) : Parser<'TState, Rune> = 
        fun (stream:TextStream<'TState>) -> 
            match stream.Next() with
            | ValueSome(r, ns) when not <| set.Contains r -> Success(ns, r)
            | _ -> Failure (stream, ParseError("", stream.Line, stream.Column))

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
                | Failure (stream, _) -> stream
            let sb = System.Text.StringBuilder()
            let nextStream = run stream sb
            if nextStream.Index <> stream.Index
            then Success (nextStream, sb.ToString())
            else Failure (stream, ParseError("", stream.Line, stream.Column))
                