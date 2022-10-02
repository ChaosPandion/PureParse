

open PureParse;
open PureParse.Examples

open System
open System.IO

let json = "{ \"array\": [ 1, 2, 3, 4, 5, 6, 7 ], \"number\": n, \"string\": \"asdasd asd sadasd\" }"
match run2 Json.parser json () with
| Success(stream, r) ->
    System.Console.ReadLine() |> ignore
| Failure(stream, er) ->
    System.Console.ReadLine() |> ignore


System.Console.ReadLine() |> ignore