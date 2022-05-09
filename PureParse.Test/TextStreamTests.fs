module TextStreamTests


open System
open System.Text
open Xunit
open PureParse.TextStream
open PureParse.Runes

type private state = { name: string }

[<Fact>]
let ``Initial Setup is correct.`` () =
    let ts = TextStream<state>.Create({ name = "test" }, "abc")
    Assert.Equal("test", ts.State.name)
    Assert.Equal(0, ts.Index)
    Assert.Equal(1, ts.Line)
    Assert.Equal(1, ts.Column)
    Assert.Equal(3, ts.Remaining)
    Assert.False(ts.IsComplete)
    Assert.Equal(new Rune('a'), ts.Rune.Value)

[<Fact>]
let ``Peek Test`` () =
    let ts = TextStream<state>.Create({ name = "test" }, "abc")
    let test () = 
        Assert.Equal("test", ts.State.name)
        Assert.Equal(0, ts.Index)
        Assert.Equal(1, ts.Line)
        Assert.Equal(1, ts.Column)
        Assert.Equal(3, ts.Remaining)
        Assert.False(ts.IsComplete)
    test ()
    match ts.Peek() with
    | ValueSome (Rune 'a') -> test ()
    | _ -> failwith "Error"
    match ts.Peek(3) with
    | ValueSome (Runes "abc") -> test ()
    | _ -> failwith "Error"

[<Fact>]
let ``UTF-32 Test`` () =
    // 10762 127753 128509
    let ts = TextStream<state>.Create({ name = "test" }, "⨊🌉🗽")
    match ts.Next(1) with
    | ValueSome (RuneString "⨊", ts1) ->
        match ts1.Next(1) with
        | ValueSome (RuneString "🌉", ts2) ->
            match ts2.Next(1) with
            | ValueSome (RuneString "🗽", ts3) -> ()
            | _ -> failwith "Error"
        | _ -> failwith "Error"
    | _ -> failwith "Error"

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

[<Fact>]
let ``Basic Line and Column Test`` () =
    let ts = TextStream<unit>.Create((), "12345\n123")
    match ts.Next(5) with 
    | ValueSome(Runes "12345", ts1) ->
        Assert.Equal(1, ts1.Line)
        Assert.Equal(6, ts1.Column)
        match ts1.Next(1) with 
        | ValueSome(Runes "\n", ts2) ->
            Assert.Equal(2, ts2.Line)
            Assert.Equal(1, ts2.Column)
            match ts2.Next(3) with 
            | ValueSome(Runes "123", ts3) ->
                Assert.Equal(2, ts3.Line)
                Assert.Equal(4, ts3.Column)    
            | _ -> failwith "Error"     
        | _ -> failwith "Error"  
    | _ -> failwith "Error"  

[<Fact>]
let ``Basic Line Replacement Test`` () =
    let ts = TextStream<unit>.Create((), "12345\r\n123")
    match ts.Next(5) with 
    | ValueSome(Runes "12345", ts1) ->
        Assert.Equal(1, ts1.Line)
        Assert.Equal(6, ts1.Column)
        match ts1.Next(1) with 
        | ValueSome(Runes "\n", ts2) ->
            Assert.Equal(2, ts2.Line)
            Assert.Equal(1, ts2.Column)
            match ts2.Next(3) with 
            | ValueSome(Runes "123", ts3) ->
                Assert.Equal(2, ts3.Line)
                Assert.Equal(4, ts3.Column)    
            | _ -> failwith "Error"     
        | _ -> failwith "Error"  
    | _ -> failwith "Error"  
