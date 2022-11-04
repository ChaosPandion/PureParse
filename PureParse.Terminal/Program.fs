

open PureParse;
open PureParse.Examples

open System
open System.IO
open System.Linq
open System.Diagnostics

(*
let accept tree = 

    let html = EventTree.createHtml tree
    System.IO.File.WriteAllText ("C:/Users/Matthew/Desktop/result.html", html)

    ()

let json = " [ 1, 2, 3, 4, [ true, false, null, [ 1, 2, 3, 4, 5, 6, 8, \"\\uaa1\"  ] ] ] "
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
*)

(*
let words = [ "true"; "false"; "null"; "nullish"; "nullite"; "nullate"; "trukish"; "then"; "thenling" ]
let pairs = Seq.allPairs words words |> Seq.toList
let substrings = 
    pairs 
    |> Seq.map (
        fun (x, y) -> 
            let length =
                Seq.init (min x.Length y.Length) (fun i -> i) 
                |> Seq.skipWhile (fun i -> x[i] = y[i]) 
                |> Seq.tryHead 
                |> Option.defaultValue 0
            if length > 0 then x.Substring(0, length) else "")
let z = 
        seq {
            for x in words do
                for y in words do
                    let mutable i = 0
                    while i < x.Length && i < y.Length && x[i] = y[i] do 
                        i <- i + 1
                    if i > 0 then
                        yield y, x.Substring(0, i) 
        }
        |> Seq.groupBy (fun (a,c) -> c)
        |> Seq.map (fun (x, y) -> x, y |> Seq.toList)
        |> Seq.filter (fun (x, y) -> y.Length > 1)
        |> Seq.map (fun (x, y) -> x, y |> Seq.distinct |> Seq.map fst |> List.ofSeq)
        |> Seq.sortByDescending (fun (x,y) -> x.Length)
        |> List.ofSeq
        
let wordsWithPrefix = z |> Seq.collect (fun (x,y) -> y)
let wordsWithoutPrefix = words |> Seq.except wordsWithPrefix
*)
open System
let x =
    """auto
    break
    case
    char		
    const
    continue
    default	
    double
    do	
    else
    enum
    extern		
    float
    for
    goto
    if		
    int
    long
    register
    return
    short
    signed
    sizeof
    static		
    struct
    switch
    typedef
    union		
    unsigned
    void
    volatile
    while""".Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries) |> Seq.map (fun s -> s.Trim()) |> Enumerable.ToList


System.Console.ReadKey(true) 
|> ignore