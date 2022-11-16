namespace rec PureParse.Test


module TextStreamTests =

    open System
    open System.Collections
    open System.Collections.Generic
    open System.Text
    open Xunit
    open PureParse
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
        Assert.Equal(new Rune('a'), ts.Value.Value)

    [<Fact>]
    let ``The state of the text stream can be set.`` () =
        let x = TextStream<state>.Create({ name = "A" }, "")
        let y = x.SetState { name = "B" }
        match y.State with
        | { name = "B" } -> ()
        | s -> failwith $"The state was not changed: {s}"

    [<Fact>]
    let ``The state of the text stream can be transformed.`` () =
        let x = TextStream<state>.Create({ name = "A" }, "")
        let y = x.TransformState (fun s -> {s with name = s.name + "B" }) 
        match y.State with
        | { name = "AB" } -> ()
        | s -> failwith $"The state was not changed: {s}"

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
    let ``Surrogate Pair Test`` () =
        let ts = TextStream<unit>.Create((), "\uD83D\uDE03")
        match ts.Next(1) with
        | ValueSome (RuneString "😃", ts) ->
            Assert.True(ts.IsComplete)
        | ValueSome (RuneString s, _) -> 
            failwith $"The value '{s}' was not expected."
        | ValueNone -> failwith "Error"

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
            Assert.Equal(5, ts1.Index)
            Assert.Equal(1, ts1.Line)
            Assert.Equal(6, ts1.Column)
            Assert.Equal(4, ts1.Remaining)
            Assert.False(ts1.IsComplete)
            match ts1.Next(1) with 
            | ValueSome(Runes "\n", ts2) ->
                Assert.Equal(6, ts2.Index)
                Assert.Equal(2, ts2.Line)
                Assert.Equal(1, ts2.Column)
                Assert.Equal(3, ts2.Remaining)
                Assert.False(ts2.IsComplete)
                match ts2.Next(3) with 
                | ValueSome(Runes "123", ts3) ->
                    Assert.Equal(9, ts3.Index)
                    Assert.Equal(2, ts3.Line)
                    Assert.Equal(4, ts3.Column)   
                    Assert.Equal(0, ts3.Remaining)
                    Assert.True(ts3.IsComplete) 
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

    [<Fact>]
    let ``The method Next(set) produces the correct result for a range of Runes.`` () =
        let ts = TextStream<unit>.Create((), "12345\n12345")
        let set = Set.ofList [ '1'; '2'; '3'; '4'; '5' ] |> Set.map Rune
        Assert.Equal(0, ts.Index)          
        Assert.Equal(1, ts.Column)        
        Assert.Equal(1, ts.Line)
        Assert.Equal(11, ts.Remaining)
        match ts.Next(set) with 
        | ValueSome (RuneString "12345", next) ->
            Assert.Equal(5, next.Index)           
            Assert.Equal(6, next.Column)     
            Assert.Equal(1, next.Line)
            Assert.Equal(6, next.Remaining)
            match next.Next() with 
            | ValueSome (Rune '\n', next) ->
                Assert.Equal(6, next.Index)             
                Assert.Equal(1, next.Column)     
                Assert.Equal(2, next.Line)
                Assert.Equal(5, next.Remaining)
                match next.Next(set) with 
                | ValueSome (RuneString "12345", next) ->                    
                    Assert.Equal(11, next.Index)                
                    Assert.Equal(6, next.Column)    
                    Assert.Equal(2, next.Line) 
                    Assert.Equal(0, next.Remaining)
                | _ -> failwith "Error" 
            | _ -> failwith "Error"   
        | _ -> failwith "Error"  

    [<Fact>]
    let ``The method Peek(set) produces the correct result for a range of Runes.`` () =
        let ts = TextStream<unit>.Create((), "12345\n12345")
        let set = Set.ofList [ '1'; '2'; '3'; '4'; '5' ] |> Set.map Rune
        Assert.Equal(0, ts.Index)          
        Assert.Equal(1, ts.Column)        
        Assert.Equal(1, ts.Line)
        Assert.Equal(11, ts.Remaining)
        match ts.Peek(set) with 
        | ValueSome (RuneString "12345") ->
            Assert.Equal(0, ts.Index)          
            Assert.Equal(1, ts.Column)        
            Assert.Equal(1, ts.Line)
            Assert.Equal(11, ts.Remaining)   
        | _ -> failwith "Error" 

    [<Fact>]
    let ``The method Next(predicate) produces the correct result for a range of Runes.`` () =
        let ts = TextStream<unit>.Create((), "12345\n12345")
        let p = Rune.IsDigit
        Assert.Equal(0, ts.Index)          
        Assert.Equal(1, ts.Column)        
        Assert.Equal(1, ts.Line)
        Assert.Equal(11, ts.Remaining)
        match ts.Next(p) with 
        | ValueSome (RuneString "12345", next) ->
            Assert.Equal(5, next.Index)           
            Assert.Equal(6, next.Column)     
            Assert.Equal(1, next.Line)
            Assert.Equal(6, next.Remaining)
            match next.Next() with 
            | ValueSome (Rune '\n', next) ->
                Assert.Equal(6, next.Index)             
                Assert.Equal(1, next.Column)     
                Assert.Equal(2, next.Line)
                Assert.Equal(5, next.Remaining)
                match next.Next(p) with 
                | ValueSome (RuneString "12345", next) ->                    
                    Assert.Equal(11, next.Index)                
                    Assert.Equal(6, next.Column)    
                    Assert.Equal(2, next.Line) 
                    Assert.Equal(0, next.Remaining)
                | _ -> failwith "Error" 
            | _ -> failwith "Error"   
        | _ -> failwith "Error"  

    [<Fact>]
    let ``The method Peek(predicate) produces the correct result for a range of Runes.`` () =
        let ts = TextStream<unit>.Create((), "12345\n12345")
        let p = Rune.IsDigit
        Assert.Equal(0, ts.Index)          
        Assert.Equal(1, ts.Column)        
        Assert.Equal(1, ts.Line)
        Assert.Equal(11, ts.Remaining)
        match ts.Peek(p) with 
        | ValueSome (RuneString "12345") ->
            Assert.Equal(0, ts.Index)          
            Assert.Equal(1, ts.Column)        
            Assert.Equal(1, ts.Line)
            Assert.Equal(11, ts.Remaining)   
        | _ -> failwith "Error" 

    [<Theory>]
    [<InlineData("AAAAA")>]
    [<InlineData("BBBBB")>]
    let ``Next(text) produces the correct result.`` text =
        let stream = TextStream<unit>.Create((), text)
        match stream.Next text with 
        | ValueSome (RuneString x, _) when x = text  -> ()
        | _ -> failwith "Error" 

    [<Theory>]
    [<InlineData("AAAAA")>]
    [<InlineData("BBBBB")>]
    let ``Peek(text) produces the correct result.`` text =
        let stream = TextStream<unit>.Create((), text)
        match stream.Peek text with 
        | ValueSome (RuneString x) when x = text  -> ()
        | _ -> failwith "Error" 

    [<Theory>]
    [<InlineData("AAAAA")>]
    [<InlineData("BBBBB")>]
    let ``Next(runes) produces the correct result.`` text =
        let stream = TextStream<unit>.Create((), text)
        let runes = text.EnumerateRunes() |> Seq.toArray |> ReadOnlyMemory
        match stream.Next runes with 
        | ValueSome (RuneString x, _) when x = text  -> ()
        | _ -> failwith "Error" 

    [<Theory>]
    [<InlineData("AAAAA")>]
    [<InlineData("BBBBB")>]
    let ``Peek(runes) produces the correct result.`` text =
        let stream = TextStream<unit>.Create((), text)
        let runes = text.EnumerateRunes() |> Seq.toArray |> ReadOnlyMemory
        match stream.Peek runes with 
        | ValueSome (RuneString x) when x = text  -> ()
        | _ -> failwith "Error" 

    [<Theory>]
    [<InlineData("12345")>]
    [<InlineData("12345 ")>]
    [<InlineData("12345  ")>]
    [<InlineData("12345\n")>]
    [<InlineData("12345A")>]
    [<InlineData("12345AAA")>]
    [<InlineData("12345AAA\n")>]
    [<InlineData("12345AAA\n\t")>]
    let ``Next(predicate) produces the correct result.`` text =
        let stream = TextStream<unit>.Create((), text)
        match stream.Next (Rune.IsDigit) with 
        | ValueSome (RuneString x, stream) -> 
            Assert.Equal("12345", x)
            Assert.Equal(5, stream.Index)
        | ValueNone -> 
            failwith "Expected the string '12345'." 

    [<Theory>]
    [<InlineData("12345")>]
    [<InlineData("12345 ")>]
    [<InlineData("12345  ")>]
    [<InlineData("12345\n")>]
    [<InlineData("12345A")>]
    [<InlineData("12345AAA")>]
    [<InlineData("12345AAA\n")>]
    [<InlineData("12345AAA\n\t")>]
    let ``Peek(predicate) produces the correct result.`` text =
        let stream = TextStream<unit>.Create((), text)
        match stream.Peek (Rune.IsDigit) with 
        | ValueSome (RuneString x) -> 
            Assert.Equal("12345", x)
            Assert.Equal(0, stream.Index)
        | ValueNone -> 
            failwith "Expected the string '12345'."  

    [<Theory>]
    [<ClassData(typedefof<RangeCheckData>)>]
    let ``Passing in Range produces the correct results.`` mode (resultMode:ResultMode) check (range:Range<int>) text result =
        let stream = TextStream<unit>.Create((), text)
        match mode with
        | Next ->
            match resultMode with
            | StreamRead ->
                match stream.Next range with 
                | ValueSome (RuneString s, stream) ->
                    Assert.Equal(result, s)  
                    match check with
                    | NoCheck -> ()
                    | StreamIsComplete complete ->
                        Assert.Equal(complete, stream.IsComplete)
                    | ReachedIndex index ->
                        Assert.Equal(index, stream.Index)         
                | ValueNone -> failwith $"Failed to read from stream."
            | StreamNotRead ->
                match stream.Next range with 
                | ValueNone -> ()
                | _ -> failwith $"Not supposed to read from stream."                
        | Peek ->
            match resultMode with
            | StreamRead ->
                match stream.Peek range with 
                | ValueSome (RuneString s) ->
                    Assert.Equal(result, s)  
                    match check with
                    | NoCheck -> ()
                    | StreamIsComplete complete ->
                        Assert.Equal(complete, stream.IsComplete)
                    | ReachedIndex index ->
                        Assert.Equal(index, stream.Index)         
                | ValueNone -> failwith $"Failed to read from stream."
            | StreamNotRead ->
                match stream.Peek range with 
                | ValueNone -> ()
                | _ -> failwith $"Not supposed to read from stream." 
                
    [<Fact>]
    let ``Peek: Given a set of 26 letters provide a string of the correct length.`` () =
        let set = (['a'..'z'] @ ['A'..'Z']) |> List.map Rune |> Set.ofList
        let expect = "AABBccDdeEffffGGG"
        let stream = TextStream<unit>.Create((), $"{expect}123456")
        match stream.Peek set with 
        | ValueSome (RuneString s) -> 
            Assert.Equal (expect, s)
        | ValueNone -> failwith "Failed to match using set."

    [<Fact>]
    let ``Next: Given a set of 26 letters provide a string of the correct length.`` () =
        let set = (['a'..'z'] @ ['A'..'Z']) |> List.map Rune |> Set.ofList
        let expect = "AABBccDdeEffffGGG"
        let stream = TextStream<unit>.Create((), $"{expect}123456")
        match stream.Next set with 
        | ValueSome (RuneString s, stream) -> 
            Assert.Equal (expect, s)
            Assert.Equal (expect.Length, stream.Index)
        | ValueNone -> failwith "Failed to match using set."


    type CallMode = Peek | Next
    type StreamCheck = NoCheck | StreamIsComplete of bool | ReachedIndex of int
    type ResultMode = StreamRead | StreamNotRead

    type RangeCheckData() =
        interface IEnumerable with
            member this.GetEnumerator() = (this :> IEnumerable<array<obj>>).GetEnumerator() :> IEnumerator
        interface IEnumerable<array<obj>> with
            member this.GetEnumerator() = 
                let basicString = "11111111111111111111" :> obj

                ([
                    // Exact match in range
                    [| Next:>obj;StreamRead;ReachedIndex 1;Exact(Bounded(1)); basicString;"1" |]
                    [| Next:>obj;StreamRead;ReachedIndex 2;Exact(Bounded(2)); basicString;"11" |]
                    [| Next:>obj;StreamRead;ReachedIndex 3;Exact(Bounded(3)); basicString;"111" |]
                    [| Next:>obj;StreamRead;ReachedIndex 4;Exact(Bounded(4)); basicString;"1111" |]
                    [| Next:>obj;StreamRead;ReachedIndex 5;Exact(Bounded(5)); basicString;"11111" |]
                    [| Next:>obj;StreamRead;ReachedIndex 6;Exact(Bounded(6)); basicString;"111111" |]
                    [| Next:>obj;StreamRead;ReachedIndex 7;Exact(Bounded(7)); basicString;"1111111" |]
                    [| Peek:>obj;StreamRead;ReachedIndex 0;Exact(Bounded(1)); basicString;"1" |]
                    [| Peek:>obj;StreamRead;ReachedIndex 0;Exact(Bounded(2)); basicString;"11" |]
                    [| Peek:>obj;StreamRead;ReachedIndex 0;Exact(Bounded(3)); basicString;"111" |]
                    [| Peek:>obj;StreamRead;ReachedIndex 0;Exact(Bounded(4)); basicString;"1111" |]
                    [| Peek:>obj;StreamRead;ReachedIndex 0;Exact(Bounded(5)); basicString;"11111" |]
                    [| Peek:>obj;StreamRead;ReachedIndex 0;Exact(Bounded(6)); basicString;"111111" |]
                    [| Peek:>obj;StreamRead;ReachedIndex 0;Exact(Bounded(7)); basicString;"1111111" |]
                    
                    // Exact match not in range
                    [| (Next:>obj); (StreamNotRead:>obj); NoCheck; (Exact(Bounded(21)):>obj); basicString; basicString|]
                    [| (Next:>obj); (StreamNotRead:>obj); NoCheck; (Exact(Bounded(22)):>obj); basicString; basicString|]
                    [| (Next:>obj); (StreamNotRead:>obj); NoCheck; (Exact(Bounded(23)):>obj); basicString; basicString|]
                    [| (Peek:>obj); (StreamNotRead:>obj); NoCheck; (Exact(Bounded(21)):>obj); basicString; basicString|]
                    [| (Peek:>obj); (StreamNotRead:>obj); NoCheck; (Exact(Bounded(22)):>obj); basicString; basicString|]
                    [| (Peek:>obj); (StreamNotRead:>obj); NoCheck; (Exact(Bounded(23)):>obj); basicString; basicString|]

                    // Read all remaining runes
                    [|  Next;StreamRead;StreamIsComplete true;Exact(Num<int>.Unbounded);basicString;basicString|]
                    [|  Next;StreamRead;StreamIsComplete true;Range<int>.Remaining;basicString;basicString|]
                    [|  Next;StreamRead;StreamIsComplete true;Between(Num<int>.Unbounded, Num<int>.Unbounded);basicString;basicString|]                    
                    [|  Peek;StreamRead;StreamIsComplete false;Exact(Num<int>.Unbounded);basicString;basicString|]
                    [|  Peek;StreamRead;StreamIsComplete false;Range<int>.Remaining;basicString;basicString|]
                    [|  Peek;StreamRead;StreamIsComplete false;Between(Num<int>.Unbounded, Num<int>.Unbounded);basicString;basicString|]
                    
                    // Read over a specified range.
                    [|  Next;StreamRead;ReachedIndex 4;Range<int>.Between(Bounded(1),Bounded(5));"1111";"1111"|]
                    [|  Next;StreamNotRead;NoCheck;Range<int>.Between(Bounded(1),Bounded(5));"";"1111"|]      
                    [|  Next;StreamRead;StreamIsComplete true;Range<int>.Between(Bounded(1),Unbounded);"1111";"1111"|] 
                    [|  Peek;StreamRead;ReachedIndex 0;Range<int>.Between(Bounded(1),Bounded(5));"1111";"1111"|]
                    [|  Peek;StreamNotRead;NoCheck;Range<int>.Between(Bounded(1),Bounded(5));"";"1111"|]      
                    [|  Peek;StreamRead;ReachedIndex 0;Range<int>.Between(Bounded(1),Unbounded);"1111";"1111"|]                
                 ] |> List.toSeq).GetEnumerator()
