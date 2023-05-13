namespace PureParse

open Parsers
open TextStream

[<AutoOpen>]
module Parse =

    /// The builder type for computational expressions. IE: parse { ... }
    [<System.Diagnostics.DebuggerStepThrough>]
    type ParseBuilder () = 

        member _.Bind (p, f) stream = 
            bind p f stream

        member _.Bind2 (p1, p2, f) stream = 
            match p1 stream with
            | Failure (_) -> Failure (stream)
            | Success (stream, v1) ->
                match p2 stream with
                | Failure (_) -> Failure (stream)
                | Success (stream, v2) -> f (v1, v2) stream

        member _.Bind3 (p1, p2, p3, f) stream = 
            match p1 stream with
            | Failure (_) -> Failure (stream)
            | Success (stream, v1) ->
                match p2 stream with
                | Failure (_) -> Failure (stream)
                | Success (stream, v2) ->
                    match p3 stream with
                    | Failure (_) -> Failure (stream)
                    | Success (stream, v3) -> f (v1, v2, v3) stream

        member _.Bind4 (p1, p2, p3, p4, f) stream = 
            match p1 stream with
            | Failure (_) -> Failure (stream)
            | Success (stream, v1) ->
                match p2 stream with
                | Failure (_) -> Failure (stream)
                | Success (stream, v2) ->
                    match p3 stream with
                    | Failure (_) -> Failure (stream)
                    | Success (stream, v3) ->
                        match p4 stream with
                        | Failure (_) -> Failure (stream)
                        | Success (stream, v4) -> f (v1, v2, v3, v4) stream            

        member _.BindReturn (p, r) (stream:TextStream<_>) =
            match p stream with
            | Success (stream, v) -> result (r v) stream
            | Failure (_) -> Failure (stream)

        member _.Bind2Return (p1, p2, r) (stream:TextStream<_>) =
            match p1 stream with
            | Failure (_) -> Failure (stream)
            | Success (stream, v1) ->
                match p2 stream with
                | Failure (_) -> Failure (stream)
                | Success (stream, v2) -> result (r (v1, v2)) stream

        member _.Bind3Return (p1, p2, p3, r) (stream:TextStream<_>) =
            match p1 stream with
            | Failure (_) -> Failure (stream)
            | Success (stream, v1) ->
                match p2 stream with
                | Failure (_) -> Failure (stream)
                | Success (stream, v2) ->
                    match p3 stream with
                    | Failure (_) -> Failure (stream)
                    | Success (stream, v3) -> result (r (v1, v2, v3)) stream

        member _.Bind4Return (p1, p2, p3, p4, r) (stream:TextStream<_>) =
            match p1 stream with
            | Failure (_) -> Failure (stream)
            | Success (stream, v1) ->
                match p2 stream with
                | Failure (_) -> Failure (stream)
                | Success (stream, v2) ->
                    match p3 stream with
                    | Failure (_) -> Failure (stream)
                    | Success (stream, v3) ->
                        match p4 stream with
                        | Failure (_) -> Failure (stream)
                        | Success (stream, v4) -> result (r (v1, v2, v3, v4)) stream

        member _.MergeSources (p1, p2) (stream:TextStream<_>) =
            match p1 stream with
            | Failure (_) -> Failure (stream)
            | Success (stream, v1) -> 
                match p2 stream with
                | Failure (_) -> Failure (stream)
                | Success (stream, v2) -> result (v1, v2) stream

        member _.Return value stream = 
            result value stream
        
        member _.Combine (a:Parser<_, unit>,b:Parser<_, _>) stream = 
            match a stream with
            | Success (stream, _) -> b stream
            | Failure (_) -> Failure (stream)

        member _.Zero () stream = 
            result () stream

        member _.ReturnFrom (p:Parser<_,_>) stream = 
            p stream

        member _.Delay delayed stream =
            delayed () stream

        member _.TryWith (p:Parser<_,_>, onWith) (stream:TextStream<_>) =
                try p stream
                with ex ->
                    stream.ReportEvent(ParseFailure(stream.CreateEventData("Failure", "An exception occurred.", ex)))
                    onWith ex stream

        member _.TryFinally (p:Parser<_,_>, onFinally) (stream:TextStream<_>) =
                try p stream
                finally onFinally ()

        member _.Run parser stream = 
            parser stream

    let parse = ParseBuilder()

    /// <summary>
    ///  Attempt to run the provided parser given a string of text and an initial state. 
    ///  The result type specifies success or failure.
    /// </summary>
    /// <exception cref="System.ArgumentNullException"><paramref name="text" /> was null.</exception>
    let tryRun<'TState, 'TResult> (parser:Parser<'TState, 'TResult>) (text:string) (state:'TState) : RunResult<'TState, 'TResult> =
        if text = null then
            nullArg (nameof(text))
        let stream = TextStream.Create(state, text)  
        stream.ReportEvent (EnterProduction(stream.CreateEventData("Root", "")))
        match parser stream with
        | Success (stream, data) -> 
            stream.ReportEvent (ExitProductionSuccess(stream.CreateEventData("Root", "")))
            stream.ReportEvent (ParseComplete(stream.CreateEventData("", "Complete")))
            let tree = stream.GetEventTree()        
            RunSuccess (stream.State, data, tree)
        | Failure (stream) -> 
            stream.ReportEvent (ExitProductionFailure(stream.CreateEventData("Root", "")))
            stream.ReportEvent (ParseComplete(stream.CreateEventData("", "Complete"))) 
            let tree = stream.GetEventTree()     
            RunFailure (stream.State, PureParseException(tree), tree)
            
    /// <summary>
    ///  Run the provided parser given a string of text and an initial state. 
    ///  When a failure occurs a detailed exception is thrown.
    /// </summary>
    /// <exception cref="System.ArgumentNullException"><paramref name="text" /> was null.</exception>
    /// <exception cref="PureParse.PureParseException"><paramref name="parser" /> did not succeed.</exception>
    let run<'TState, 'TResult> (parser:Parser<'TState, 'TResult>) (text:string) (state:'TState) : 'TResult =
        if text = null then
            nullArg (nameof(text))
        match tryRun parser text state with
        | RunSuccess (_, data, _) -> data
        | RunFailure (_, error, _) -> raise error