

open PureParse;
open PureParse.Examples

open System
open System.IO

let accept tree = 

    let html = EventTree.createHtml tree
    System.IO.File.WriteAllText ("C:/Users/Matthew/Desktop/result.html", html)

    ()

let json = " [ 1, 2, 3, 4, [ true, false, null, [ 1, 2, 3, 4, 5, 6, 8  ]  ] "
//let json = System.IO.File.ReadAllText ("C:/Users/Matthew/Desktop/turkish.json")
match tryRun Json.parser json () with
| RunSuccess(state, data, tree) ->
    accept tree
    printfn "Success"
    System.Console.ReadLine() 
    |> ignore
| RunFailure(state, error, tree) ->
    accept tree
    let d = EventTree.getDeepestFailure tree
    let x = error.Message
    System.Console.WriteLine (d)
    printfn "Failure"
    System.Console.ReadLine() 
    |> ignore


System.Console.ReadLine() |> ignore