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


    let run (parser) (text) (state) =
        let stream = TextStream.Create(state, text)
        match parser stream with
        | Success (stream, result) -> result
        | Failure (stream, error) -> 
            printfn "%O" error
            raise error


    let run2<'TState, 'TResult> (parser:Parser<'TState, 'TResult>) (text:string) (state:'TState) : Result<'TState, 'TResult> * EventTree<'TState> =
        if text = null then
            nullArg (nameof(text))
        let stream = TextStream.Create(state, text)  
        match parser stream with
        | Success (stream, _) as result -> 
            stream.ReportEvent (ParseComplete(stream.CreateEventData("", "")))
            let tree = stream.GetEventTree()        
            result, tree
        | Failure (stream, error) as result -> 
            stream.ReportEvent (ParseComplete({ stream.CreateEventData("", "") with error = error })) 
            let tree = stream.GetEventTree()          
            result, tree