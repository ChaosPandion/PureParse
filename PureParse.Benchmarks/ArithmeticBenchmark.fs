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
type ArithmeticBenchmark () =
    class        
        [<Benchmark>]
        member _.EvalSmallExpression () =
            Arithmetic.evalText "1 + 1"

        [<Benchmark>]
        member _.EvalBigExpression () =
            Arithmetic.evalText "1 + 1 + 1000 * 20000 / 30000 + 2"

        [<Benchmark>]
        member _.EvalDeepExpression () =
            Arithmetic.evalText "1 + 1 + (1000 * (1000 * (1000 * (1000 * (1000 * 20000 / 30000) / 30000) / 30000) / 30000) / 30000) + 2"
    end

