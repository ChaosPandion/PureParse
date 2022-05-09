namespace PureParse

open System
open System.Text
open FSharp.NativeInterop
open System.Numerics;
open System.Runtime.Intrinsics;
open Runes


open TextStream

module Parsers =

    /// The 'bind' function of the monad
    let bind (parser: Parser<_, _>) (transform: Transform<_, _, _>) state = 
        match parser state with
        | Success (state, value) -> 
            transform value state
        | Failure (_, message) -> 
            Failure (state, message)
            
    /// The 'bind' function of the monad
    let (>>=) p f s = bind p f s

    /// The 'pipe' function of the monad
    let (>>) p1 p2 state = p1 >>= (fun _ -> p2) state

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
        Failure (state, ParseError(message, state.Line, state.Column)) 

    /// This is a 'choice' combinator.
    let (<|>) p1 p2 state =
        match p1 state with
        | Success (_, _) as result -> result
        | Failure (state, _) -> p2 state

    /// This parser is always a success returning an Option value
    let optional (parser:M<_, _>) state = 
        match parser state with
        | Success (state, value) -> Success (state, Some value) 
        | Failure (_, _) -> Success (state, None)
        
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
            | None -> Failure (state, ParseError("", state.Line, state.Column))
            
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

    /// Parse 'p' until your reach the end parser 'e'
    let parseUntil<'a> (p:M<_, _>) (e:M<_, unit>) state : Result<_, 'a list> =
        let first = p state 
        match first with
        | Failure (state, message) -> 
            Failure (state, message)
        | Success (state, first) ->
            let a = ResizeArray<'a>()
            a.Add (first)
            let rec parse state =
                let inline complete () = a.ToArray() |> List.ofArray
                match e state with
                | Success (state, _) -> 
                    result (complete ()) state
                | Failure (state, _) -> 
                    match p state with
                    | Failure (state, message) -> 
                        Failure (state, message)
                    | Success (state, _) ->
                        parse state
            parse state 

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

    let parseList<'TState, 'a> (parser:M<'TState, 'a>) (separator:M<'TState, unit>) allowTrailingSeparator state :Result<'TState, 'a list> =
        let first = parser state 
        match first with
        | Failure (state, message) -> 
            Failure (state, message)
        | Success (state, first) ->
            let a = ResizeArray<'a>()
            a.Add (first)
            let rec parse state =
                let inline complete () = a.ToArray() |> List.ofArray
                match separator state with
                | Failure (state, _) -> 
                    result (complete ()) state
                | Success (state, _) -> 
                    match parser state with
                    | Failure (state, message) -> 
                        if allowTrailingSeparator 
                        then result (complete ()) state
                        else Failure (state, message)
                    | Success (state, next) ->
                        a.Add(next)
                        parse state
            parse state

