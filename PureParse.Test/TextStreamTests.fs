module TextStreamTests


open System
open System.Text
open Xunit
open PureParse.TextStream
open PureParse.Runes


[<Fact>]
let ``Basic Test of Rune TextStream With No Custom State`` () =
    let ts = TextStream<unit>.Create((), "abc")
    match ts.Next() with
    | ValueSome(Rune 'a', ts1) when ts1.Index = 1 ->
        match ts1.Next() with
        | ValueSome(Rune "b", ts2) when ts2.Index = 2 ->
            match ts2.Next() with
            | ValueSome(Rune "c" as c, ts3) when ts3.Index = 3 -> ()
            | _ -> failwith "The third call to next was not successful"
        | _ -> failwith "The second call to next was not successful"
    | _ -> failwith "The first call to next was not successful"

[<Fact>]
let ``Test Rune Sequence of TextStream`` () =
    let ts = TextStream<unit>.Create((), "abcdefhij")
    match ts.Next(6) with
    | ValueSome(Runes "abcdef", ts1) when ts1.Index = 6 ->
        match ts1.Next(3) with
        | ValueSome(Runes "hij", ts2) when ts2.Index = 9 -> ()       
        | _ -> failwith "The second call to next was not successful"     
    | _ -> failwith "The first call to next was not successful"


