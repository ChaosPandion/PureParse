

open PureParse;
open PureParse.Examples

open System
open System.IO

let accept tree = 

    let html = EventTree.createHtml tree
    System.IO.File.WriteAllText ("C:/Users/Matthew/Desktop/result.html", html)

    ()

let json = "[ 123, true, false, { \"a\": f } ]"
match run2 Json.parser json () accept with
| Success(stream, r) ->
    System.Console.ReadLine() 
    |> ignore
| Failure(stream, er) ->
    System.Console.ReadLine() 
    |> ignore


System.Console.ReadLine() |> ignore