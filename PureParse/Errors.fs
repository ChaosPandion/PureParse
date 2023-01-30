namespace PureParse

open System

/// A specific error at a location in the text stream.
exception ParseError of message:string * index:int * line:int * column:int
    with override this.ToString() = $"A failure occurred '{this.message}' at index = {this.index}, line = {this.line}, column = {this.column}"

/// The exception that is thrown when a parser does not succeed.
type PureParseException<'TState>(tree:EventTree<'TState>) =
    inherit Exception()

    let message = lazy(
        let rec build (tree:EventTree<'TState>) depth =
            let d = 
                match tree with
                | SucceededProduction d -> d
                | FailedProduction d -> d
            let n = String.replicate depth "    "
            let m = n + $"Production: {d.enterData.parserName}:\r\n"
            let x = (d.failures |> List.map(fun f -> " " + n + f.error.Value.ToString() + "\r\n"))
            let x = List.fold (fun x y -> x + y) "" x
            let c = d.children |> Seq.map (fun z -> build z (depth + 1)) 
            let c = Seq.fold (fun x y -> x + y) "" c
            m + x + c
        build tree 0)

    /// Gets the event tree associated with the failed parsing operation.
    member _.Tree = tree

    /// Gets the exception message.
    override _.Message = message.Value

