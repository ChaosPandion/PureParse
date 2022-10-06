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
        let stream, mailbox = TextStream.Create(state, text, fun tree -> ())
        use _ = mailbox
        match parser stream with
        | Success (stream, result) -> result
        | Failure (stream, error) -> 
            printfn "%O" error
            raise error

    let run2<'TState, 'TResult> (parser:Parser<'TState, 'TResult>) (text:string) (state:'TState) =
        if text = null then
            nullArg (nameof(text))
        let channel = Channel.CreateBounded<EventTree<'TState>>(1)
        let accept tree =
            use writeTask = task { 
                do! channel.Writer.WriteAsync(tree); 
                channel.Writer.Complete(); 
            } 
            writeTask.Wait()
        let stream, mailbox = TextStream.Create(state, text, accept)            
        use _ = mailbox
        use parseTask = task {
            match parser stream with
            | Success (stream, _) as r -> 
                stream.ReportEvent (ParseComplete(stream.CreateEventData("", "")))
                let! tree = channel.Reader.ReadAsync()            
                return r, tree
            | Failure (stream, error) as r -> 
                stream.ReportEvent (ParseComplete({ stream.CreateEventData("", "") with error = error })) 
                let! tree = channel.Reader.ReadAsync()          
                return r, tree
        }
        parseTask.Wait()
        parseTask.Result