

open PureParse;
open PureParse.Examples

open System
open System.IO

let accept tree = 

    ()

let json = "{ \"number\": 123 }"
match run2 Json.parser json () accept with
| Success(stream, r) ->
    System.Console.ReadLine() 
    |> ignore
| Failure(stream, er) ->
    System.Console.ReadLine() 
    |> ignore


System.Console.ReadLine() |> ignore