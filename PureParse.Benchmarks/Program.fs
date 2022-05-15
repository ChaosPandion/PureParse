module Main 

open BenchmarkDotNet
open BenchmarkDotNet.Running

let [<EntryPoint>] main args =
    let t = typeof<PureParse.Benchmarks.JsonBenchmark>
    BenchmarkSwitcher.FromAssembly(t.Assembly).Run(args) |> ignore
    0