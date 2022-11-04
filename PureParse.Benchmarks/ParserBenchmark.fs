namespace PureParse.Benchmarks 

open System
open BenchmarkDotNet
open BenchmarkDotNet.Attributes
open PureParse
open PureParse.Examples
open System.Runtime.InteropServices;
open BenchmarkDotNet.Diagnostics.Windows.Configs;

[<CategoriesColumn>]
[<NativeMemoryProfiler>]
[<MemoryDiagnoser>]
type ParserBenchmark () =
    
    let words = [ "true"; "false"; "nullish"; "nullite"; "nullate"; "trukish"; "then"; "thenling"; "null" ]
    let p1 = parseAnyString<unit> words
        
    [<Benchmark>]
    member _.ParseAnyStringTest () =
        match tryRun p1 "nullate" () with
        | RunSuccess (_, "nullate", _) -> ()
        | RunSuccess (_, a, _) -> failwith $"Unknown Result: {a}"
        | _ -> failwith "Unknown Result"
        match tryRun p1 "trukish" () with
        | RunSuccess (_, "trukish", _) -> ()
        | RunSuccess (_, a, _) -> failwith $"Unknown Result: {a}"
        | _ -> failwith "Unknown Result"
        match tryRun p1 "true" () with
        | RunSuccess (_, "true", _) -> ()
        | RunSuccess (_, a, _) -> failwith $"Unknown Result: {a}"
        | _ -> failwith "Unknown Result"
