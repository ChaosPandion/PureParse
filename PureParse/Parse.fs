namespace PureParse

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
        let stream, m = TextStream.Create(state, text, fun tree -> ())
        use _ = m
        match parser stream with
        | Success (stream, result) -> result
        | Failure (stream, error) -> 
            printfn "%O" error
            raise error
    let run2 (parser) (text:string) (state) acceptEventTree =
        let stream, m = TextStream.Create(state, text, acceptEventTree)
        use _ = m
        match parser stream with
        | Success (stream, result) as r -> 
            stream.ReportEvent (ParseComplete(stream.CreateEventData("", "")))
            r
        | Failure (stream, error) as r -> 
            stream.ReportEvent (ParseComplete(stream.CreateEventData("", "")))
            r