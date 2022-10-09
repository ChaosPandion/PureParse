namespace PureParse

open System

exception ParseError of message:string * index:int * line:int * column:int
    with override this.ToString() = $"A failure occurred '{this.message}' at index = {this.index}, line = {this.line}, column = {this.column}"

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

    member _.Tree = tree
    override _.Message = message.Value

