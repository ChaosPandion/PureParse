namespace PureParse.Benchmarks 

open System
open BenchmarkDotNet
open BenchmarkDotNet.Attributes
open PureParse
open PureParse.Examples
open System.Runtime.InteropServices;
open BenchmarkDotNet.Diagnostics.Windows.Configs;
open Number

[<CategoriesColumn>]
[<NativeMemoryProfiler>]
[<MemoryDiagnoser>]
type NumberBenchmark () =

    [<Benchmark>]
    member _.ParseInt64PureParse () =
        let text = Int64.MaxValue.ToString()
        let result = parseInt64 text
        let _ = result + 1L
        ()

    [<Benchmark>]
    member _.ParseInt64System () =
        let text = Int64.MaxValue.ToString()
        let result = Int64.Parse text
        let _ = result + 1L
        ()

    [<Benchmark>]
    member _.ParseDoublePureParse () =
        let text = "-10000000.1002e+1"
        let result = parseDouble text
        let _ = result + 1.0
        ()

    [<Benchmark>]
    member _.ParseDoubleSystem () =
        let text = "-10000000.1002e+1"
        let result = Double.Parse text
        let _ = result + 1.0
        ()
