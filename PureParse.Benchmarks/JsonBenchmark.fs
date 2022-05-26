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
[<AnyCategoriesFilter("Number")>]
type JsonBenchmark () =
    // [<GlobalSetup>]
    // member self.GlobalSetup() =
    //     printfn "%s" "Global Setup"

    // [<GlobalCleanup>]
    // member self.GlobalCleanup() =
    //     printfn "%s" "Global Cleanup"

    // [<IterationSetup>]
    // member self.IterationSetup() =
    //     printfn "%s" "Iteration Setup"
    
    // [<IterationCleanup>]
    // member self.IterationCleanup() =
    //     printfn "%s" "Iteration Cleanup"

    [<Benchmark>]
    [<BenchmarkCategory("Simple", "Number")>]
    member this.ParseMaxInt32 () =
        let text = "2147483647"
        let result = Json.parseText text
        match result with
        | Json.JsonNumber n -> ()           
        | _ -> failwith "Unexpected type"
    [<Benchmark>]
    [<BenchmarkCategory("Simple", "Number")>]
    member this.ParseMaxDouble () =
        let text = "1.7976931348623157E+308"
        let result = Json.parseText text
        match result with
        | Json.JsonNumber n -> ()           
        | _ -> failwith "Unexpected type"
    
    [<Benchmark>]
    [<BenchmarkCategory("Simple", "Boolean")>]
    member this.ParseBoolean () =
        let text = "true"
        let result = Json.parseText text
        match result with
        | Json.JsonBoolean true -> ()
        | Json.JsonBoolean false -> ()            
        | _ -> failwith "Unexpected type"

    [<Benchmark>]
    [<BenchmarkCategory("Simple", "Null")>]
    member this.ParseNull () =
        let text = "null"
        let result = Json.parseText text
        match result with
        | Json.JsonNull -> ()         
        | _ -> failwith "Unexpected type"

    [<Benchmark>]
    [<BenchmarkCategory("Simple", "String")>]
    member this.ParseEmptyString () =
        let text = "\"\""
        let result = Json.parseText text
        match result with
        | Json.JsonString s -> ()           
        | _ -> failwith "Unexpected type"
        
    [<Benchmark>]
    [<BenchmarkCategory("Simple", "String")>]
    member this.ParseSimpleString () =
        let text = "\"aaa\""
        let result = Json.parseText text
        match result with
        | Json.JsonString s -> ()           
        | _ -> failwith "Unexpected type"
        
    [<Benchmark>]
    [<BenchmarkCategory("Simple", "String")>]
    member this.ParseEscapedString () =
        let text = "\"\\b\\t\\n\\r\""
        let result = Json.parseText text
        match result with
        | Json.JsonString s -> ()           
        | _ -> failwith "Unexpected type"

    [<Benchmark>]
    [<BenchmarkCategory("Complex", "Object")>]
    member this.ParseEmptyObject () =
        let text = "{}"
        let result = Json.parseText text
        match result with
        | Json.JsonObject map -> ()
        | _ -> failwith "Unexpected type"

    [<Benchmark>]
    [<BenchmarkCategory("Complex", "Object")>]
    member this.ParseOneMemberObject () =
        let text = """{ "aaa": 123  }"""
        let result = Json.parseText text
        match result with
        | Json.JsonObject map -> ()
        | _ -> failwith "Unexpected type"

    [<Benchmark>]
    [<BenchmarkCategory("Complex", "Object")>]
    member this.ParseTwoMemberObject () =
        let text = """{ "aaa": 123, "bbb": true  }"""
        let result = Json.parseText text
        match result with
        | Json.JsonObject map -> ()
        | _ -> failwith "Unexpected type"

    [<Benchmark>]
    [<BenchmarkCategory("Complex", "Object")>]
    member this.ParseThreeMemberObject () =
        let text = """{ "aaa": 123, "bbb": true, "ccc": null   }"""
        let result = Json.parseText text
        match result with
        | Json.JsonObject map -> ()
        | _ -> failwith "Unexpected type"

    [<Benchmark>]
    [<BenchmarkCategory("Complex", "Object")>]
    member this.ParseRecursiveObject () =
        let text = """{ "aaa": 123, "bbb": true, "ccc": { "aaa": 123, "bbb": true, "ccc": { "aaa": 123, "bbb": true, "ccc": null   }   }   }"""
        let result = Json.parseText text
        match result with
        | Json.JsonObject map -> ()
        | _ -> failwith "Unexpected type"

    [<Benchmark>]
    [<BenchmarkCategory("Complex", "Array")>]
    member this.ParseEmptyArray () =
        let text = "[]"
        let result = Json.parseText text
        match result with
        | Json.JsonArray items -> ()
        | _ -> failwith "Unexpected type"

    [<Benchmark>]
    [<BenchmarkCategory("Complex", "Array")>]
    member this.ParseOneElementArray () =
        let text = "[ 123 ]"
        let result = Json.parseText text
        match result with
        | Json.JsonArray items -> ()
        | _ -> failwith "Unexpected type"

    [<Benchmark>]
    [<BenchmarkCategory("Complex", "Array")>]
    member this.ParseTwoElementArray () =
        let text = "[ 123, true ]"
        let result = Json.parseText text
        match result with
        | Json.JsonArray items -> ()
        | _ -> failwith "Unexpected type"

    [<Benchmark>]
    [<BenchmarkCategory("Complex", "Array")>]
    member this.ParseThreeElementArray () =
        let text = "[ 123, true, \"aaa\" ]"
        let result = Json.parseText text
        match result with
        | Json.JsonArray items -> ()
        | _ -> failwith "Unexpected type"

    [<Benchmark>]
    [<BenchmarkCategory("Complex", "Array")>]
    member this.ParseRecursiveArray () =
        let text = "[ 123, true, [ 123, true, [ 123, true, [ 123, true, \"aaa\" ] ] ] ]"
        let result = Json.parseText text
        match result with
        | Json.JsonArray items -> ()
        | _ -> failwith "Unexpected type"

    [<Benchmark>]
    [<BenchmarkCategory("Complex", "Array and Object")>]
    member this.ParseRecursiveObjectArray () =
        let text = """[ 123, { "aaa": 123, "bbb": true, "ccc": null   }, [ 123, { "aaa": 123, "bbb": true, "ccc": null   }, [ 123, true, [ 123, { "aaa": 123, "bbb": true, "ccc": null   }, "aaa" ] ] ] ]"""
        let result = Json.parseText text
        match result with
        | Json.JsonArray items -> ()
        | _ -> failwith "Unexpected type"
