namespace PureParse

open Parsers

[<AutoOpen>]
module Parse =

    type ParseBuilder () = 

        member _.Bind (p:Parser<_, _>, f:Transform<_, _, _>):Parser<_, _> = 
            fun state -> bind p f state

        member _.Return (value):Parser<_, _> = 
            fun state -> result value state

        member _.Zero ():Parser<_, _> = 
            fun state -> result () state

        member _.ReturnFrom (p:Parser<_, _>):Parser<_, _> = 
            fun state -> p state

        member _.Delay (delayed:Delayed<_, _>) : Parser<_, _> = 
            fun state -> delayed () state

        member _.Run (parser:Parser<_, _>) : Parser<_, _> = 
            fun state -> parser state

    let parse = ParseBuilder()


    let run (parser) (text) (state) =
        let stream = TextStream.TextStream.Create(state, text)
        match parser stream with
        | Success (_, result) -> result
        | Failure (_, error) -> 
            printfn "%O" error
            raise error