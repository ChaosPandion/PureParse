namespace PureParse

open System
open System.Diagnostics
open System.Text
open FSharp.NativeInterop
open System.Numerics;
open System.Runtime.Intrinsics;

[<AutoOpen>]
module Parsers =

    /// Bind the provided parser to the provided transform resulting in a new parser.
    [<DebuggerStepThrough>]
    let bind<'TState, 'TData1, 'TData2> (parser: Parser<'TState, 'TData1>) (transform: Transform<'TState, 'TData1, 'TData2>) : Parser<'TState, 'TData2> =
        fun stream ->
            match parser stream with
            | Success (stream, value) -> 
                transform value stream
            | Failure (_) -> 
                Failure(stream) 

    /// The 'return' or 'unit' function of the monad
    [<DebuggerStepThrough>]
    let result<'TState, 'TData> (value: 'TData) : Parser<'TState, 'TData> = 
        fun stream ->
            Success (stream, value) 

    /// Evaluate the provided parser and transform the result using the provided mapping function.
    let map<'TState, 'TData1, 'TData2> (parser: Parser<'TState, 'TData1>) (f:'TData1 -> 'TData2) : Parser<'TState, 'TData2> =
        fun stream ->
            match parser stream with
            | Success (nextStream, value) -> 
                try
                    Success (nextStream, f value) 
                with 
                | e ->
                    nextStream.ReportEvent(ParseFailure(nextStream.CreateErrorEventData("Map Failure", e.ToString())))
                    Failure (stream)
            | Failure (_) -> 
                Failure (stream)

    /// Given two parsers evaluate both and return the result from the left parser.
    let sequenceLeft<'TState, 'TData1, 'TData2> (left:Parser<'TState, 'TData1>) (right:Parser<'TState, 'TData2>) : Parser<'TState, 'TData1> = 
        fun stream ->
            match left stream with
            | Success (s2, data) -> 
                match right s2 with
                | Success (s3, _) -> 
                    Success (s3, data)
                | Failure (_) -> 
                    Failure (stream)
            | Failure (_) -> 
                Failure (stream)

    /// Given two parsers evaluate both and return the result from the right parser.
    let sequenceRight<'TState, 'TData1, 'TData2> (left:Parser<'TState, 'TData1>) (right:Parser<'TState, 'TData2>) : Parser<'TState, 'TData2> = 
        fun stream ->
            match left stream with
            | Success (stream, _) -> right stream 
            | Failure (_) -> Failure (stream)

    /// Given a list of parsers, evaluate them in sequence returning a 
    /// list containing the results of each parser,
    let sequence<'TState, 'TResult> (parsers:Parser<'TState, 'TResult> list) 
        : Parser<'TState, 'TResult list>  = 
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
                let m = "The provided parsers did not all succeed."
                let e = stream.CreateErrorEventData("Sequence", m)
                stream.ReportEvent(ParseFailure(e))
                Failure(stream)
            else 
                Success(nextStream, result)

    /// Evaulate the two provided parsers and return the result as a tuple.
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

    /// Evaulate the three provided parsers and return the result as a tuple.
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

    /// Evaluate the four provided parsers and return the result as a 4-tuple.
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

    /// Given a list of parsers evaluate each in order and return the first success.
    let choose<'TState, 'TResult> (parsers:Parser<'TState, 'TResult> list) : Parser<'TState, 'TResult> =
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

    /// Given two parsers evaluate each in order and return the first success.
    let choose2<'TState, 'TResult> (left:Parser<'TState, 'TResult>) (right:Parser<'TState, 'TResult>) : Parser<'TState, 'TResult> =
        fun stream ->
            match left stream with
            | Success (_, _) as result -> result
            | Failure (_) -> right stream            

    /// Given three parsers evaluate each in order and return the first success.
    let choose3<'TState, 'TResult> 
        (p1:Parser<'TState, 'TResult>) 
        (p2:Parser<'TState, 'TResult>) 
        (p3:Parser<'TState, 'TResult>) : Parser<'TState, 'TResult> =
        fun stream ->
            match p1 stream with
            | Success (_, _) as result -> result
            | Failure (_) ->
                match p2 stream with
                | Success (_, _) as result -> result
                | Failure (_) ->
                    match p3 stream with
                    | Success (_, _) as result -> result
                    | Failure (_) -> Failure (stream)

    /// Given four parsers evaluate each in order and return the first success.
    let choose4<'TState, 'TResult> 
        (p1:Parser<'TState, 'TResult>) 
        (p2:Parser<'TState, 'TResult>) 
        (p3:Parser<'TState, 'TResult>)
        (p4:Parser<'TState, 'TResult>) : Parser<'TState, 'TResult> =
        fun stream ->
            match p1 stream with
            | Success (_, _) as result -> result
            | Failure (_) ->
                match p2 stream with
                | Success (_, _) as result -> result
                | Failure (_) ->
                    match p3 stream with
                    | Success (_, _) as result -> result
                    | Failure (_) ->
                        match p4 stream with
                        | Success (_, _) as result -> result
                        | Failure (_) -> Failure (stream)
          
    /// Raises a ParseFailure event with the provided message and always returns a Failure result.
    let failWithMessage<'TState, 'TData> (message:string) : Parser<'TState, 'TData> =
        if message = null then nullArg (nameof(message))
        fun (stream:TextStream<'TState>) ->
            stream.ReportEvent(ParseFailure(stream.CreateErrorEventData("Failure", message)))
            Failure (stream) 

    /// Provided with a new state return Success with a new TextStream that contains the new state
    let setState<'TState> (nextState:'TState) : Parser<'TState, unit> =
        fun (stream:TextStream<'TState>) ->
            Success (stream.SetState nextState, ())
            
    /// Provided with a function that takes the current state and returns the next state, 
    /// return Success with a new TextStream that contains the new state.
    let transformState<'TState> (transform:'TState -> 'TState) : Parser<'TState, unit> =
        fun (stream:TextStream<'TState>) ->
            Success (stream.TransformState transform, ())

    /// This parser is always a success returning an Option value
    let optional<'TState, 'TData> (parser:Parser<'TState, 'TData>) : Parser<'TState, 'TData option> =
        fun (stream:TextStream<'TState>) ->
            match parser stream with
            | Success (stream, value) -> 
                Success (stream, Some value) 
            | Failure (_) -> 
                Success (stream, None)
                
    /// The provided parser is evaluated and the result is always a success
    let succeedIgnoringResult<'TState, 'TData> (parser: Parser<'TState, 'TData>) : Parser<'TState, unit> =
        fun stream ->
            match parser stream with
            | Success(stream, _) -> result () stream
            | Failure(_) -> result () stream   

    /// When the provided parser is a success the result will always be unit.
    let ignoreResultWhenSuccess<'TState, 'TData> (parser: Parser<'TState, 'TData>) : Parser<'TState, unit> =
        fun stream ->
            match parser stream with
            | Success(stream, _) -> Success (stream, ())  
            | Failure(_) -> Failure (stream)  
    
    /// The provided parser is evaluated and the result is always a success.
    let omit<'TState, 'TData> (parser: Parser<'TState, 'TData>) : Parser<'TState, unit> = succeedIgnoringResult parser

    /// Evaluate the next rune using the provided predicate and return the result.
    let satisfy<'TState> (predicate:Rune -> bool) =
        fun (stream:TextStream<'TState>) ->
            match stream.Next() with
            | ValueSome (r, stream) when predicate r -> Success (stream, r) 
            | _ -> 
                stream.ReportEvent(ParseFailure(stream.CreateErrorEventData("Failure", "Did not satisfy predicate")))
                Failure (stream)  
        
    /// This parser is always a success returning an Option value. The state of the computation is unchanged.
    let peek<'TState, 'TResult> (parser:Parser<'TState, 'TResult>) : Parser<'TState, option<'TResult>> = 
        fun (stream: TextStream<'TState>) ->
            match parser stream with
            | Success (_, value) -> 
                Success (stream, Some value) 
            | Failure (_) -> 
                Success (stream, None)
            
    /// When the provided parser is a success the result will always be unit.
    let skip<'TState, 'TResult> (parser:Parser<'TState, 'TResult>) : Parser<'TState, unit> = ignoreResultWhenSuccess parser        

    /// Evaluate all of the provided parsers and ignore the results.
    let skipSequence<'TState, 'TResult> (parsers:Parser<'TState, 'TResult> list) : Parser<'TState, unit>  = omit (sequence parsers)

    /// Pass over skip and evaluate parser.
    let skipParse<'TState, 'TResult1, 'TResult2> (s:Parser<'TState, 'TResult1>) (parser:Parser<'TState, 'TResult2>) : Parser<'TState, 'TResult2> =
        fun (stream: TextStream<'TState>) ->
            match s stream with
            | Success(stream, _) -> parser stream
            | Failure(_) -> parser stream

    /// The parser 'p' is given a formal name that is provided when an error occurs.
    let provideName<'TState, 'TResult> (p: Parser<'TState, 'TResult>) (name:string) : Parser<'TState, 'TResult> =
        fun (stream: TextStream<'TState>) ->
            match p stream with
            | Success (_, _) as result -> result
            | Failure (stream) -> 
                stream.ReportEvent(ParseFailure(stream.CreateErrorEventData(name, $"Parse Failed")))
                Failure (stream)

    /// The parser 'p' is given a formal name and description that is provided when an error occurs.
    let provideNameAndDescription<'TState, 'TResult> (p: Parser<'TState, 'TResult>) (name:string, description:string) : Parser<'TState, 'TResult> =
        fun (stream: TextStream<'TState>) ->
            match p stream with
            | Success (_, _) as result -> result
            | Failure (stream) -> 
                stream.ReportEvent(ParseFailure(stream.CreateErrorEventData(name, description)))
                Failure (stream)

    /// This function is the primary way to organize the event stream into productions.
    let parseProduction<'TState, 'TResult> (name: string) (parser: Parser<'TState, 'TResult>) : Parser<'TState, 'TResult> = 
        fun (stream: TextStream<'TState>) ->
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

    /// This parser is a success when the stream is complete.            
    let parseEnd<'TState> () : Parser<'TState, unit> =
        fun stream -> if stream.IsComplete then Success (stream, ()) else Failure (stream)

    /// This parser will evaluate the provided parser and return the result but will not move the stream forward.
    let lookAhead<'TState, 'TData> (parser:Parser<'TState, 'TData>) : Parser<'TState, 'TData> =
        fun stream ->
            match parser stream with
            | Success (_, data) -> Success (stream, data)
            | Failure (_) -> Failure (stream)

    let (>>=) = bind
    let (||>) = map
    let (<-|) = sequenceLeft
    let (|->) = sequenceRight
    let (<|>) = choose2    
    let (<?>) = provideName
    let (<??>) = provideNameAndDescription