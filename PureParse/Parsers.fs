﻿namespace PureParse

open System
open System.Text
open FSharp.NativeInterop
open System.Numerics;
open System.Runtime.Intrinsics;

[<AutoOpen>]
module Parsers =

    /// The 'bind' function of the monad
    let bind (parser: Parser<_, _>) (transform: Transform<_, _, _>) stream = 
        match parser stream with
        | Success (stream, value) -> 
            transform value stream
        | Failure (_, message) -> 
            Failure (stream, message)
            
    /// The 'bind' function of the monad
    let (>>=) p f s = bind p f s

    /// The 'pipe' function of the monad
    let (>>) p1 p2 state = p1 >>= (fun _ -> p2) state

    /// The parser 'p' is given a formal name that is provided when an error occurs.
    let (<?>) p (name:string) =
        fun stream ->
            match p stream with
            | Success (_, _) as result -> result
            | Failure (stream, error) -> 
                let e = NamedParserError (name, "", error)
                Failure (stream, e)

    /// The parser 'p' is given a formal name and description that is provided when an error occurs.
    let (<??>) p (name:string, description:string) =
        fun stream ->
            match p stream with
            | Success (_, _) as result -> result
            | Failure (stream, error) -> 
                let e = NamedParserError (name, description, error)
                Failure (stream, e)


    let ``⊕`` x y : Parser<_, _> =
        fun stream ->
            match x stream with
            | Success (_, _) as result -> result
            | Failure (_, _) -> y stream

    let (<|>) = ``⊕``
    let alternative = ``⊕``
    let choose2 = ``⊕``
    let choose3 x y z = ``⊕`` (``⊕`` x y) z 

    let sequenceLeft x y = 
        fun stream ->
            match x stream with
            | Success (s2, data) -> 
                match y s2 with
                | Success (s3, _) -> 
                    Success (s3, data)
                | Failure (_, error) -> 
                    Failure (stream, error)
            | Failure (_, error) -> 
                Failure (stream, error)

    let sequenceRight x y = 
        fun stream ->
            match x stream with
            | Success (stream, _) -> y stream 
            | Failure (_, error) -> Failure (stream, error)

    let ``⊛`` = sequenceRight
    let sequencing = ``⊛``
    
    let (|->) = sequenceRight
    let (<-|) = sequenceLeft

    let map p f stream =
        match p stream with
        | Success (stream, value) -> 
            Success (stream, f value) 
        | Failure (_, error) -> 
            Failure (stream, error)

    /// The 'return' or 'unit' function of the monad
    let result value state = 
        Success (state, value) 

    /// The 'return' or 'unit' function of the monad
    let fail message state = 
        Failure (state, state.CreateFailure message ParseError) 


    /// This parser is always a success returning an Option value
    let optional (parser:M<_, _>) state = 
        match parser state with
        | Success (state, value) -> Success (state, Some value) 
        | Failure (_, _) -> Success (state, None)

    let satisfy<'TState> (predicate:Rune -> bool) =
        fun (stream:TextStream<'TState>) ->
            match stream.Next() with
            | ValueSome (r, stream) when predicate r -> Success (stream, r) 
            | _ -> Failure(stream, stream.CreateFailure "Did not satisfy." ParseError)
        
    /// This parser is always a success returning an Option value. The state of the computation is unchanged.
    let peek (parser:M<_, _>) state = 
        match parser state with
        | Success (_, value) -> Success (state, Some value) 
        | Failure (_, _) -> Success (state, None)

    /// This parser is always a success returning an Option value
    let opt (parser:M<_, _>) state = optional parser state

    /// This is a 'choice' combinator.
    let choose<'TState, 'a> (parsers:Parser<'TState, 'a> list) state =
        match parsers with
        | [] -> failwith "No parsers provided."            
        | [ p1; p2 ] -> (p1 <|> p2) state
        | _ ->
            let parse p  = 
                async { match p state with
                        | Success (_, _) as result -> 
                            return Some result
                        | Failure (_, _) -> 
                            return None }
            let a = 
                parsers 
                |> Seq.map parse 
                |> Async.Choice 
                |> Async.RunSynchronously
            match a with
            | Some n -> n
            | None -> Failure (state, state.CreateFailure "None of the parsers succeeded." ParseError)

    let chooseSync<'TState, 'TResult> (parsers:Parser<'TState, 'TResult> list) =
        fun (stream:TextStream<'TState>) ->
            let rec loop ps =
                match ps with
                | [] -> Failure (stream, stream.CreateFailure "No parser succeeded." ParseError)
                | p::ps ->
                    match p stream with
                    | Success (_, _) as result -> result
                    | Failure (_, _) -> loop ps
            loop parsers

            
    /// Pass over skip and evaluate parser.
    let skip (parser:M<_, _>) state =
        match parser state with
        | Success(state, _)
        | Failure(state, _) -> result () state

    /// Pass over skip and evaluate parser.
    let skipParse (skip:M<_, _>) (parser:M<_, _>) state =
        match skip state with
        | Success(state, _) -> parser state
        | Failure(state, _) -> parser state

    let sequence<'a> (parsers:Parser<_, 'a> list) : Parser<_, 'a list>  = 
        fun (state) ->
            let rec parse ps state rs =
                match ps with
                | [] -> Success (state, rs |> List.rev)
                | p::ps -> 
                    match p state with
                    | Success (state, v) ->
                        parse ps state (v::rs)
                    | Failure (_, message) ->
                        Failure (state, message)
            parse parsers state []

    let skipSequence<'a> (parsers:Parser<_, 'a> list) : Parser<_, unit>  = 
        fun (state) ->
            let rec parse ps state =
                match ps with
                | [] -> Success (state, ())
                | p::ps -> 
                    match p state with
                    | Success (state, _) ->
                        parse ps state
                    | Failure (_, message) ->
                        Failure (state, message)
            parse parsers state 