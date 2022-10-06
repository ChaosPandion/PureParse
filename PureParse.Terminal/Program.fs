

open PureParse;
open PureParse.Examples

open System
open System.IO

let accept tree = 

    let html = EventTree.createHtml tree
    System.IO.File.WriteAllText ("C:/Users/Matthew/Desktop/result.html", html)

    ()

//let json = "{ \"aaaa\": \"asdasdasdasdasdasdasdasdasdasdsa } "
let json = System.IO.File.ReadAllText ("C:/Users/Matthew/Desktop/reddit.json")
match run2 Json.parser json () with
| Success(stream, r), tree ->
    accept tree
    System.Console.ReadLine() 
    |> ignore
| Failure(stream, er), tree ->
    accept tree
    System.Console.ReadLine() 
    |> ignore


System.Console.ReadLine() |> ignore