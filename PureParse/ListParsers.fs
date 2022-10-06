namespace rec PureParse

open System
open System.Text
open FSharp.NativeInterop
open System.Numerics;
open System.Runtime.Intrinsics;

[<AutoOpen>]
module ListParsers =

    type ListMode<'TState, 'TData> =
        | Many of minElements:int
        | Until of ending:Parser<'TState, unit>
        | Sep of separator:Parser<'TState, unit> * allowTrailingSeparator:bool * minElements:int

    let parseList<'TState, 'TData> 
        (parser:Parser<'TState, 'TData>) 
        (mode:ListMode<'TState, 'TData>) 
        : Parser<'TState, 'TData list> =

        match mode with
        | Many minElements -> 
            parseMany parser minElements 
        | Until ending -> 
            parseUntil parser ending
        | Sep (separator, allowTrailingSeparator, minElements) -> 
            parseManySep parser separator allowTrailingSeparator minElements

    let parseMany<'TState, 'TElement> 
        (parser:Parser<'TState, 'TElement>) 
        (minElements:int)
        : Parser<'TState, List<'TElement>> =

        fun (stream:TextStream<'TState>) ->
            let rec parse stream elements =
                match parser stream with
                | Failure (_, _) -> struct(stream, elements)
                | Success (stream, element) -> 
                    let struct(stream, elements) = parse stream elements
                    struct(stream, element::elements)
            let struct(stream, elements) = parse stream []
            if elements.Length < minElements then 
                let m = sprintf "At least %i elements are required." minElements
                Failure (stream, stream.CreateFailure m ParseError)
            else
                Success(stream, elements)

    let parseUntil<'TState, 'TElement> 
        (parser:Parser<'TState, 'TElement>) 
        (ending:Parser<'TState, unit>)
        : Parser<'TState, List<'TElement>> =

        fun (stream:TextStream<'TState>) ->
            let firstStream = stream
            let rec parse stream elements =
                match parser stream with
                | Failure (_, _) -> 
                    struct (firstStream, [], false)
                | Success (stream, element) ->
                    match ending stream with
                    | Success (stream, _) ->
                        struct (stream, element::elements, true)
                    | Failure (_, _) ->
                        let struct(stream, elements, success) = parse stream elements
                        struct(stream, element::elements, success)
            let struct(stream, elements, success) = parse stream []
            if not success then 
                let m = "Failed to reach ending parser."
                Failure (firstStream, stream.CreateFailure m ParseError)
            else
                Success(stream, elements)

    let parseManySep<'TState, 'TElement> 
        (parser:Parser<'TState, 'TElement>) 
        (separator:Parser<'TState, unit>) 
        (allowTrailingSeparator:bool) 
        (minElements:int) 
        : Parser<'TState, List<'TElement>> =


        fun (stream:TextStream<'TState>) ->
            let firstStream = stream
            let rec parse stream elements =
                match parser stream with
                | Failure (_, _) -> 
                    struct (stream, [], false)
                | Success (stream, element) ->
                    match separator stream with
                    | Success (stream, _) ->
                        let struct(stream, elements, success) = parse stream elements
                        if not success then
                            struct(stream, element::elements, allowTrailingSeparator)
                        else
                            struct(stream, element::elements, true)

                    | Failure (_, _) ->
                        struct(stream, element::elements, true)
            let struct(stream, elements, success) = parse stream []
            if not success && (minElements = 0 && elements.Length = 0) then
                Success (stream, elements)
            elif not success || elements.Length < minElements then 
                let m = sprintf "Failed to create a list of at least %i elements." minElements
                Failure (firstStream, stream.CreateFailure m ParseError)
            else
                Success (stream, elements)