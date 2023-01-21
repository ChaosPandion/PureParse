namespace rec PureParse

open System
open System.Text
open FSharp.NativeInterop
open System.Numerics;
open System.Runtime.Intrinsics;

[<AutoOpen>]
module ListParsers =

    /// The list parsing mode and the parameters they require.
    type ListMode<'TState, 'TData> =
        | Many of minElements:int
        | Until of ending:Parser<'TState, unit> * minElements:int
        | Sep of separator:Parser<'TState, unit> * allowTrailingSeparator:bool * minElements:int
        | SepUntil of separator:Parser<'TState, unit> * ending:Parser<'TState, unit> * allowTrailingSeparator:bool *  minElements:int
        | ExactCount of count:int
        
    /// Evaluate the parser using the provided mode, collecting the results.
    let inline parseList<'TState, 'TData> 
        (parser:Parser<'TState, 'TData>) 
        (mode:ListMode<'TState, 'TData>) 
        : Parser<'TState, 'TData list> =
        match mode with
        | Many minElements -> 
            parseMany parser minElements 
        | Until (ending, minElements) -> 
            parseUntil parser ending minElements
        | Sep (separator, allowTrailingSeparator, minElements) -> 
            parseManySep parser separator allowTrailingSeparator minElements
        | SepUntil (separator, ending, allowTrailingSeparator, minElements) -> 
            parseManySepUntil parser separator ending allowTrailingSeparator minElements
        | ExactCount count -> 
            parseExactCount parser count

    /// Evaluate the parser collecting the results. The result must have the exact count of elements expected.
    let parseExactCount<'TState, 'TElement> (parser:Parser<'TState, 'TElement>) (count:int) : Parser<'TState, List<'TElement>> = 
        fun (stream:TextStream<'TState>) ->
            let elements = ResizeArray(count)
            let mutable currentStream = stream
            let mutable complete = false
            while not complete do
                if elements.Count = count then
                    complete <- true
                else
                    match parser currentStream with
                    | Success (nextStream, element) ->
                        currentStream <- nextStream
                        elements.Add element
                    | Failure (_) ->
                        complete <- true
            let incorrectAmountOfElements = elements.Count <> count
            if incorrectAmountOfElements then
                if incorrectAmountOfElements then
                    stream.ReportEvent(ParseFailure(currentStream.CreateErrorEventData("Exact Count", $"Expected {count} elements.")))
                stream.ReportEvent(ParseFailure(stream.CreateErrorEventData("Exact Count", $"The list must have exactly {count} elements")))
                Failure (stream)
            else
                Success (currentStream, elements |> Seq.toList)

    /// Evaluate the parser collecting the results.
    let parseMany<'TState, 'TElement> 
        (parser:Parser<'TState, 'TElement>) 
        (minElements:int)
        : Parser<'TState, List<'TElement>> =
        fun (stream:TextStream<'TState>) ->
            let elements = ResizeArray()
            let mutable currentStream = stream
            let mutable complete = false
            while not complete do
                match parser currentStream with
                | Success (nextStream, element) ->
                    currentStream <- nextStream
                    elements.Add element
                | Failure (_) ->
                    complete <- true
            let notEnoughElements = elements.Count < minElements
            if notEnoughElements then
                if notEnoughElements then
                    stream.ReportEvent(ParseFailure(currentStream.CreateErrorEventData("Many", $"Expected {minElements} elements.")))
                stream.ReportEvent(ParseFailure(stream.CreateErrorEventData("Many", "Failed to parse list of elements.")))
                Failure (stream)
            else
                Success (currentStream, elements |> Seq.toList)

    /// Evaluate the parser until the ending, collecting the results.
    let parseUntil<'TState, 'TElement> 
        (parser:Parser<'TState, 'TElement>) 
        (ending:Parser<'TState, unit>)
        (minElements:int)
        : Parser<'TState, List<'TElement>> =
        fun (stream:TextStream<'TState>) ->
            let elements = ResizeArray()
            let mutable currentStream = stream
            let mutable complete = false
            let mutable failure = false
            while not complete do
                match parser currentStream with
                | Success (nextStream, element) ->
                    currentStream <- nextStream
                    elements.Add element
                    match ending currentStream with
                    | Success (nextStream, _) ->
                        currentStream <- nextStream
                        complete <- true
                    | Failure (_) -> ()
                | Failure (_) ->
                    match ending currentStream with
                    | Success (nextStream, _) ->
                        currentStream <- nextStream
                    | Failure (_) ->
                        failure <- true
                    complete <- true
            let notEnoughElements = elements.Count < minElements
            if failure || notEnoughElements then
                if failure then
                    stream.ReportEvent(ParseFailure(currentStream.CreateErrorEventData("Until", $"Failed to find ending.")))
                if notEnoughElements then
                    stream.ReportEvent(ParseFailure(currentStream.CreateErrorEventData("Until", $"Expected {minElements} elements.")))
                stream.ReportEvent(ParseFailure(stream.CreateErrorEventData("Until", "Failed to parse list of elements.")))
                Failure (stream)
            else
                Success (currentStream, elements |> Seq.toList)

    /// Evaluate the parser separated by the separator, collecting the results.
    let parseManySep<'TState, 'TElement> 
        (parser:Parser<'TState, 'TElement>) 
        (separator:Parser<'TState, unit>) 
        (allowTrailingSeparator:bool) 
        (minElements:int) 
        : Parser<'TState, List<'TElement>> =
        fun (stream:TextStream<'TState>) ->
            let elements = ResizeArray()
            let mutable currentStream = stream
            let mutable complete = false
            let mutable trailingSeparatorNotAllowed = false
            while not complete do
                match parser currentStream with
                | Success (nextStream, element) ->
                    currentStream <- nextStream
                    elements.Add element
                    match separator nextStream with
                    | Success (nextStream, _) -> 
                        currentStream <- nextStream
                    | Failure (_) ->
                        complete <- true
                | Failure (_) ->
                    trailingSeparatorNotAllowed <- elements.Count > 1 && not allowTrailingSeparator
                    complete <- true
            let notEnoughElements = elements.Count < minElements
            if trailingSeparatorNotAllowed || notEnoughElements then
                if trailingSeparatorNotAllowed then
                    stream.ReportEvent(ParseFailure(currentStream.CreateErrorEventData("Many Sep", "No trailing separator is allowed.")))
                if notEnoughElements then
                    stream.ReportEvent(ParseFailure(currentStream.CreateErrorEventData("Many Sep", $"Expected {minElements} elements.")))
                stream.ReportEvent(ParseFailure(stream.CreateErrorEventData("Many Sep", "Failed to parse list of elements.")))
                Failure (stream)
            else
                Success (currentStream, elements |> Seq.toList)

    /// Evaluate the parser separated by the separator until the ending, collecting the results.
    let parseManySepUntil<'TState, 'TElement> 
        (parser:Parser<'TState, 'TElement>) 
        (separator:Parser<'TState, unit>) 
        (ending:Parser<'TState, unit>) 
        (allowTrailingSeparator:bool) 
        (minElements:int) 
        : Parser<'TState, List<'TElement>> =
        fun (stream:TextStream<'TState>) ->
            let elements = ResizeArray()
            let mutable currentStream = stream
            let mutable complete = false
            let mutable trailingSeparatorNotAllowed = false
            let mutable failedToFindEndingPastSeparator = false
            while not complete do
                match parser currentStream with
                | Success (nextStream, element) ->
                    currentStream <- nextStream
                    elements.Add element
                    match ending nextStream with
                    | Success (nextStream, _) ->                            
                        currentStream <- nextStream
                        complete <- true
                    | Failure (_) ->
                        match separator nextStream with
                        | Success (nextStream, _) -> 
                            currentStream <- nextStream
                        | Failure (_) ->
                            complete <- true
                | Failure (_) ->
                    if elements.Count > 1 && not allowTrailingSeparator then
                        trailingSeparatorNotAllowed <- true
                    else
                        match ending currentStream with
                        | Success (nextStream, _) ->                            
                            currentStream <- nextStream
                        | Failure (_) ->
                            failedToFindEndingPastSeparator <- true
                    complete <- true
            let notEnoughElements = elements.Count < minElements
            if trailingSeparatorNotAllowed || notEnoughElements || failedToFindEndingPastSeparator then
                if trailingSeparatorNotAllowed then
                    stream.ReportEvent(ParseFailure(currentStream.CreateErrorEventData("Many Sep Until", "No trailing separator is allowed.")))
                if notEnoughElements then
                    stream.ReportEvent(ParseFailure(currentStream.CreateErrorEventData("Many Sep Until", $"Expected {minElements} elements.")))
                if failedToFindEndingPastSeparator then
                    stream.ReportEvent(ParseFailure(currentStream.CreateErrorEventData("Many Sep Until", "No ending was found past the trailing separator.")))
                stream.ReportEvent(ParseFailure(stream.CreateErrorEventData("Many Sep Until", "Failed to parse list of elements.")))
                Failure (stream)
            else
                Success (currentStream, elements |> Seq.toList)


    // Pure versions

    /// Evaluate the parser collecting the results.
    let parseMany2<'TState, 'TElement> 
        (parser:Parser<'TState, 'TElement>) 
        (minElements:int)
        : Parser<'TState, List<'TElement>> =
        fun (stream:TextStream<'TState>) ->
            let rec parse stream elements =
                match parser stream with
                | Failure (_) -> struct(stream, elements)
                | Success (stream, element) -> 
                    let struct(stream, elements) = parse stream elements
                    struct(stream, element::elements)
            let struct(stream, elements) = parse stream []
            if elements.Length < minElements then 
                let m = sprintf "At least %i elements are required." minElements
                stream.ReportEvent(ParseFailure(stream.CreateErrorEventData("Parse Many", m)))
                Failure(stream) 
            else
                Success(stream, elements)

    /// Evaluate the parser until the ending, collecting the results.
    let parseUntil2<'TState, 'TElement> 
        (parser:Parser<'TState, 'TElement>) 
        (ending:Parser<'TState, unit>)
        : Parser<'TState, List<'TElement>> =
        fun (stream:TextStream<'TState>) ->
            let firstStream = stream
            let rec parse stream elements =
                match parser stream with
                | Failure (_) -> 
                    struct (firstStream, [], false)
                | Success (stream, element) ->
                    match ending stream with
                    | Success (stream, _) ->
                        struct (stream, element::elements, true)
                    | Failure (_) ->
                        let struct(stream, elements, success) = parse stream elements
                        struct(stream, element::elements, success)
            let struct(stream, elements, success) = parse stream []
            if not success then 
                let m = "Failed to reach ending parser."
                stream.ReportEvent(ParseFailure(stream.CreateErrorEventData("Parse Until", m)))
                Failure(stream) 
            else
                Success(stream, elements)

    /// Evaluate the parser separated by the separator, collecting the results.
    let parseManySep2<'TState, 'TElement> 
        (parser:Parser<'TState, 'TElement>) 
        (separator:Parser<'TState, unit>) 
        (allowTrailingSeparator:bool) 
        (minElements:int) 
        : Parser<'TState, List<'TElement>> =
        fun (stream:TextStream<'TState>) ->
            let rec parse stream elements =
                match parser stream with
                | Failure (_) -> 
                    struct (stream, [], false)
                | Success (stream, element) ->
                    match separator stream with
                    | Success (stream, _) ->
                        let struct(stream, elements, success) = parse stream elements
                        if not success then
                            struct(stream, element::elements, allowTrailingSeparator)
                        else
                            struct(stream, element::elements, true)

                    | Failure (_) ->
                        struct(stream, element::elements, true)
            let struct(stream, elements, success) = parse stream []
            if not success && (minElements = 0 && elements.Length = 0) then
                Success (stream, elements)
            elif not success || elements.Length < minElements then 
                let m = sprintf "Failed to create a list of at least %i elements." minElements
                stream.ReportEvent(ParseFailure(stream.CreateErrorEventData("Parse Many Sep", m)))
                Failure(stream) 
            else
                Success (stream, elements)

    /// Evaluate the parser separated by the separator until the ending, collecting the results.
    let parseManySepUntil2<'TState, 'TElement> 
        (parser:Parser<'TState, 'TElement>) 
        (separator:Parser<'TState, unit>) 
        (ending:Parser<'TState, unit>) 
        (minElements:int) 
        : Parser<'TState, List<'TElement>> =
        fun (stream:TextStream<'TState>) ->
            let firstStream = stream
            let rec parse stream elements =
                match parser stream with
                | Failure (_) -> 
                    struct (firstStream, [], false)
                | Success (stream, element) ->
                    match ending stream with
                    | Success (stream, _) ->
                        struct(stream, element::elements, true)
                    | Failure (stream) ->
                        match separator stream with
                        | Failure (_) ->
                            struct(stream, element::elements, true)
                        | Success (stream, _) ->
                            let struct(stream, elements, success) = parse stream elements
                            struct(stream, element::elements, success)
            let struct(stream, elements, success) = parse stream []
            if not success && (minElements = 0 && elements.Length = 0) then
                Success (stream, elements)
            elif not success || elements.Length < minElements then 
                let m = sprintf "Failed to create a list of at least %i elements." minElements
                stream.ReportEvent(ParseFailure(stream.CreateErrorEventData("Parse Many Sep Until", m)))
                Failure(stream) 
            else
                Success (stream, elements)