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

    /// If the provided parser fails report an event to the stream with the provided message.
    let onFailure<'TState, 'TData> (message:string) (parser:Parser<'TState, 'TData>) : Parser<'TState, 'TData> =
        if message = null then nullArg (nameof(message))
        fun (stream:TextStream<'TState>) ->
            match parser stream with
            | Success (_, _) as r -> r
            | Failure (_) ->
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

            
    /// This parser is always a success.
    let skip<'TState, 'TResult> (parser:Parser<'TState, 'TResult>) : Parser<'TState, unit> =
        fun stream ->
            match parser stream with
            | Success(stream, _) -> result () stream
            | Failure(_) -> result () stream

    /// Pass over skip and evaluate parser.
    let skipParse (skip:M<_, _>) (parser:M<_, _>) =
        fun stream ->
            match skip stream with
            | Success(stream, _) -> parser stream
            | Failure(_) -> parser stream

    let sequenceA<'a> (parsers:Parser<_, 'a> list) : Parser<_, 'a list>  = 
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

    let sequence<'TState, 'TResult> (parsers:Parser<'TState, 'TResult> list) : Parser<_, 'TResult list>  = 
        fun stream ->
            let rec parse ps stream rs =
                match ps with
                | [] -> rs, stream
                | p::ps -> 
                    match p stream with
                    | Success (stream, v) ->
                        let rs, stream = parse ps stream rs
                        v::rs, stream
                    | Failure (_) -> [], stream
            let result, nextStream = parse parsers stream []
            if result.Length <> parsers.Length then
                stream.ReportEvent(ParseFailure(stream.CreateErrorEventData("Sequence", "The provided parsers did not all succeed.")))
                Failure(stream)
            else 
                Success(nextStream, result)

    let sequence2<'TState, 'TResult1, 'TResult2> 
        (p1:Parser<'TState, 'TResult1>) 
        (p2:Parser<'TState, 'TResult2>) : Parser<'TState, 'TResult1 * 'TResult2>  = 
        fun stream ->
            match p1 stream with
            | Success (stream2, r1) ->
                match p2 stream2 with
                | Success (stream3, r2) ->
                    Success (stream3, (r1, r2))
                | Failure (_) -> Failure (stream)
            | Failure (_) -> Failure (stream)

    let sequence3<'TState, 'TResult1, 'TResult2, 'TResult3> 
        (p1:Parser<'TState, 'TResult1>) 
        (p2:Parser<'TState, 'TResult2>) 
        (p3:Parser<'TState, 'TResult3>) : Parser<'TState, 'TResult1 * 'TResult2 * 'TResult3>  = 
        fun stream ->
            match p1 stream with
            | Success (stream2, r1) ->
                match p2 stream2 with
                | Success (stream3, r2) ->
                    match p3 stream3 with
                    | Success (stream4, r3) ->
                            Success (stream4, (r1, r2, r3))
                    | Failure (_) -> Failure (stream)
                | Failure (_) -> Failure (stream)
            | Failure (_) -> Failure (stream)

    let sequence4<'TState, 'TResult1, 'TResult2, 'TResult3, 'TResult4> 
        (p1:Parser<'TState, 'TResult1>) 
        (p2:Parser<'TState, 'TResult2>) 
        (p3:Parser<'TState, 'TResult3>) 
        (p4:Parser<'TState, 'TResult4>) : Parser<'TState, 'TResult1 * 'TResult2 * 'TResult3 * 'TResult4>  = 
        fun stream ->
            match p1 stream with
            | Success (stream2, r1) ->
                match p2 stream2 with
                | Success (stream3, r2) ->
                    match p3 stream3 with
                    | Success (stream4, r3) ->
                        match p4 stream4 with
                        | Success (stream5, r4) ->
                            Success (stream5, (r1, r2, r3, r4))
                        | Failure (_) -> Failure (stream)
                    | Failure (_) -> Failure (stream)
                | Failure (_) -> Failure (stream)
            | Failure (_) -> Failure (stream)

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
                
