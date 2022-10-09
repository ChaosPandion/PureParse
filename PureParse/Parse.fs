namespace PureParse

open System.Threading.Channels
open Parsers
open TextStream

[<AutoOpen>]
module Parse =

    type ParseBuilder () = 

        member _.Bind (p:Parser<_, _>, f:Transform<_, _, _>):Parser<_, _> = 
            fun stream -> bind p f stream

        member _.Return (value):Parser<_, _> = 
            fun stream -> result value stream

        member _.Zero ():Parser<_, _> = 
            fun stream -> result () stream

        member _.ReturnFrom (p:Parser<_, _>):Parser<_, _> = 
            fun stream -> p stream

        member _.Delay (delayed:Delayed<_, _>) : Parser<_, _> = 
            fun stream -> delayed () stream

        member _.Run (parser:Parser<_, _>) : Parser<_, _> = 
            fun stream -> parser stream

    let parse = ParseBuilder()

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

    let run<'TState, 'TResult> (parser:Parser<'TState, 'TResult>) (text:string) (state:'TState) : 'TResult =
        match tryRun parser text state with
        | RunSuccess (_, data, _) -> data
        | RunFailure (_, error, _) -> raise error