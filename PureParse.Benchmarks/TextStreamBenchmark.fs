namespace PureParse.Benchmarks 

open System
open System.Text
open BenchmarkDotNet
open BenchmarkDotNet.Attributes
open PureParse
open PureParse.Examples
open System.Runtime.InteropServices;
open BenchmarkDotNet.Diagnostics.Windows.Configs;

[<CategoriesColumn>]
[<NativeMemoryProfiler>]
[<MemoryDiagnoser>]
type TextStreamBenchmark () =
    class        
        let mutable text = ""

        [<GlobalSetup>]
        member _.GenerateString () =
            let sb = System.Text.StringBuilder()
            let y = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\r\n"
            for i = 0 to 4000 do
                sb.Append (y) |> ignore
            text <- sb.ToString()

        [<Benchmark>]
        member _.CreateTextStream () =
            let ts = TextStream.Create ((), text)
            ts

        [<Benchmark>]
        member _.IterateOneByOne () =
            let mutable ts = TextStream.Create ((), text)
            while not ts.IsComplete do
                match ts.Next() with
                | ValueSome (_, next) -> 
                    ts <- next
                | _ -> ()
            ts

        [<Benchmark>]
        member _.IterateTwoByTwo () =
            let mutable ts = TextStream.Create ((), text)
            while not ts.IsComplete do
                match ts.Next(2) with
                | ValueSome (_, next) -> 
                    ts <- next
                | _ -> 
                    match ts.Next() with
                    | ValueSome (_, next) -> 
                        ts <- next
                    | _ -> ()
            ts

        [<Benchmark>]
        member _.IterateByPredicate () =
            let mutable ts = TextStream.Create ((), text)
            let runeA = Rune('a')
            let runeB = Rune('b')
            while not ts.IsComplete do
                match ts.Next(fun r -> r = runeA) with
                | ValueSome (_, next) -> 
                    ts <- next
                | _ -> 
                    match ts.Next(fun r -> r = runeB) with
                    | ValueSome (_, next) -> 
                        ts <- next
                    | _ ->
                        match ts.Next(Range.Remaining) with
                        | ValueSome (_, next) -> 
                            ts <- next
                        | _ -> ()
            ts
    end



