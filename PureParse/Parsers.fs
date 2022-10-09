namespace PureParse

open System
open System.Text
open FSharp.NativeInterop
open System.Numerics;
open System.Runtime.Intrinsics;

[<AutoOpen>]
module Parsers =

    /// The 'bind' function of the monad
    let bind<'TState, 'TData1, 'TData2> (parser: Parser<'TState, 'TData1>) (transform: Transform<'TState, 'TData1, 'TData2>): Parser<'TState, 'TData2> =
        fun stream ->
            match parser stream with
            | Success (stream, value) -> 
                transform value stream
            | Failure (s) as r -> Failure(stream)  
            
    /// The 'bind' function of the monad
    let (>>=) p f s = bind p f s

    /// The 'pipe' function of the monad
    let (>>) p1 p2 state = p1 >>= (fun _ -> p2) state

    /// The parser 'p' is given a formal name that is provided when an error occurs.
    let (<?>) p (name:string) =
        fun stream ->
            match p stream with
            | Success (_, _) as result -> result
            | Failure (stream) -> 
                stream.ReportEvent(ParseFailure(stream.CreateErrorEventData(name, $"Parse Failed")))
                Failure (stream)

    /// The parser 'p' is given a formal name and description that is provided when an error occurs.
    let (<??>) p (name:string, description:string) =
        fun stream ->
            match p stream with
            | Success (_, _) as result -> result
            | Failure (stream) -> 
                stream.ReportEvent(ParseFailure(stream.CreateErrorEventData(name, description)))
                Failure (stream)


    let ``⊕`` x y : Parser<_, _> =
        fun stream ->
            match x stream with
            | Success (_, _) as result -> result
            | Failure (_) -> y stream

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
                | Failure (_) -> 
                    Failure (stream)
            | Failure (_) -> 
                Failure (stream)

    let sequenceRight x y = 
        fun stream ->
            match x stream with
            | Success (stream, _) -> y stream 
            | Failure (_) -> Failure (stream)

    let ``⊛`` = sequenceRight
    let sequencing = ``⊛``
    
    let (|->) = sequenceRight
    let (<-|) = sequenceLeft

    let map p f stream =
        match p stream with
        | Success (stream, value) -> 
            Success (stream, f value) 
        | Failure (_) -> 
            Failure (stream)

    /// The 'return' or 'unit' function of the monad
    let result value state = 
        Success (state, value) 

    /// The 'return' or 'unit' function of the monad
    let fail<'TState, 'TData> (message:string) : Parser<'TState, 'TData> =
        fun (stream:TextStream<'TState>) ->
            stream.ReportEvent(ParseFailure(stream.CreateErrorEventData("Failure", message)))
            Failure (stream) 


    /// This parser is always a success returning an Option value
    let optional (parser:M<_, _>) state = 
        match parser state with
        | Success (state, value) -> Success (state, Some value) 
        | Failure (_) -> Success (state, None)

    let satisfy<'TState> (predicate:Rune -> bool) =
        fun (stream:TextStream<'TState>) ->
            match stream.Next() with
            | ValueSome (r, stream) when predicate r -> Success (stream, r) 
            | _ -> 
                stream.ReportEvent(ParseFailure(stream.CreateErrorEventData("Failure", "Did not satisfy predicate")))
                Failure (stream)  
        
    /// This parser is always a success returning an Option value. The state of the computation is unchanged.
    let peek (parser:M<_, _>) state = 
        match parser state with
        | Success (_, value) -> Success (state, Some value) 
        | Failure (_) -> Success (state, None)

    /// This parser is always a success returning an Option value
    let opt<'TState, 'TData> (parser:Parser<'TState, 'TData>):Parser<'TState, 'TData option> =
        fun (stream:TextStream<'TState>) ->
            match parser stream with
            | Success (state, value) -> Success (state, Some value) 
            | Failure (_) -> Success (stream, None)

    /// This is a 'choice' combinator.
    let choose<'TState, 'a> (parsers:Parser<'TState, 'a> list) (stream:TextStream<'TState>) =
        match parsers with
        | [] -> failwith "No parsers provided."            
        | [ p1; p2 ] -> (p1 <|> p2) stream
        | _ ->
            let parse p  = 
                async { match p stream with
                        | Success (_, _) as result -> 
                            return Some result
                        | Failure (_) -> 
                            return None }
            let a = 
                parsers 
                |> Seq.map parse 
                |> Async.Choice 
                |> Async.RunSynchronously
            match a with
            | Some n -> n
            | None -> 
                stream.ReportEvent(ParseFailure(stream.CreateErrorEventData("Choose", "None of the parsers succeeded.")))
                Failure (stream)  

    let chooseSync<'TState, 'TResult> (parsers:Parser<'TState, 'TResult> list) =
        fun (stream:TextStream<'TState>) ->
            let rec loop (stream:TextStream<'TState>) ps =
                match ps with
                | [] ->
                    stream.ReportEvent(ParseFailure(stream.CreateErrorEventData("Failure", "None of the parsers succeeded.")))
                    Failure (stream)  
                | p::ps ->
                    match p stream with
                    | Success (_, _) as result -> result
                    | Failure (_) -> loop stream ps
            loop stream parsers

            
    /// Pass over skip and evaluate parser.
    let skip (parser:M<_, _>) state =
        match parser state with
        | Success(state, _)
        | Failure(state) -> result () state

    /// Pass over skip and evaluate parser.
    let skipParse (skip:M<_, _>) (parser:M<_, _>) state =
        match skip state with
        | Success(state, _) -> parser state
        | Failure(state) -> parser state

    let sequence<'a> (parsers:Parser<_, 'a> list) : Parser<_, 'a list>  = 
        fun (state) ->
            let rec parse ps state rs =
                match ps with
                | [] -> Success (state, rs |> List.rev)
                | p::ps -> 
                    match p state with
                    | Success (state, v) ->
                        parse ps state (v::rs)
                    | Failure (_) ->
                        Failure (state)
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
                    | Failure (_) ->
                        Failure (state)
            parse parsers state 


    let parseProduction<'TState, 'TResult> (name: string) (parser: Parser<'TState, 'TResult>) : Parser<'TState, 'TResult> = 
        fun (stream) ->
            let ev = EnterProduction(stream.CreateEventData(name)) 
            stream.ReportEvent ev         
            match parser stream with
            | Success (stream, v) ->                
                let ev = ExitProductionSuccess(stream.CreateEventData(name))  
                stream.ReportEvent ev
                Success(stream, v)
            | Failure (stream) ->  
                let ev = ExitProductionFailure(stream.CreateEventData(name))  
                stream.ReportEvent ev
                Failure(stream)
                
