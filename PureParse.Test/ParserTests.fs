namespace PureParse.Test

open System
open System.Text
open PureParse
open Xunit

module ParserTests = begin
        
        [<Fact>]
        let ``bind() is a success`` () =
            let test x = 
                let p:Parser<unit, char> = fun stream -> Success (stream, '1')
                let t:Transform<unit, char, int> = fun c stream -> Success(stream, string c |> int)
                let stream = TextStream.Create((), "1")
                let f = x p t stream
                match f with
                | Success (_, i) -> Assert.Equal(1, i)
                | Failure (_) -> failwith "Expecting Success"
            test bind
            test (>>=)

        [<Fact>]
        let ``bind() fails to apply t when p is a failure.`` () =
            let test x = 
                let p:Parser<unit, char> = fun stream -> Failure (stream)
                let t:Transform<unit, char, int> = fun c stream -> Success(stream, string c |> int)
                let stream = TextStream.Create((), "1")
                let f = x p t stream
                match f with
                | Failure (_) -> ()
                | Success (_, _) -> failwith "Expecting Failure"
            test bind
            test (>>=)
            
        [<Theory>]
        [<InlineData(1)>]
        [<InlineData(2)>]
        [<InlineData(3)>]
        [<InlineData("a")>]
        let ``Test the result function`` x = 
            let stream = TextStream.Create((), "1")
            match result x stream with
            | Success (_, x) -> ()
            | _ -> failwith "Expecting Success"

        [<Fact>]
        let ``Test the map parser`` () =
            let p:Parser<unit, char> = 
                fun stream -> 
                    match stream.Next() with
                    | ValueSome(Rune '1', s) -> Success (s, '1')
                    | _ -> Failure (stream)
            let m x = string x |> int
            let stream = TextStream.Create((), "1")
            let f = map p m stream
            match f with
            | Success (_, 1) -> ()
            | _ -> failwith "Expecting Success"
            let f = (p ||> m) stream
            match f with
            | Success (_, 1) -> ()
            | _ -> failwith "Expecting Success"
        
        [<Fact>]
        let ``Test the sequenceRight parser operator`` () =
            let p1:Parser<unit, char> = 
                fun stream -> 
                    match stream.Next() with
                    | ValueSome(Rune '1', s) -> Success (s, '1')
                    | _ -> Failure (stream)
            let p2:Parser<unit, char> = 
                fun stream -> 
                    match stream.Next() with
                    | ValueSome(Rune '2', s) -> Success (s, '2')
                    | _ -> Failure (stream)
            let stream = TextStream.Create((), "12")
            let f = sequenceRight p1 p2 stream
            match f with
            | Success (_, '2') -> ()
            | _ -> failwith "Expecting Success"
            let f = (p1 |-> p2) stream
            match f with
            | Success (_, '2') -> ()
            | _ -> failwith "Expecting Success"
        
        [<Fact>]
        let ``Test the sequenceLeft parser operator`` () =
            let p1:Parser<unit, char> = 
                fun stream -> 
                    match stream.Next() with
                    | ValueSome(Rune '1', s) -> Success (s, '1')
                    | _ -> Failure (stream)
            let p2:Parser<unit, char> = 
                fun stream -> 
                    match stream.Next() with
                    | ValueSome(Rune '2', s) -> Success (s, '2')
                    | _ -> Failure (stream)
            let stream = TextStream.Create((), "12")
            let f = sequenceLeft p1 p2 stream
            match f with
            | Success (_, '1') -> ()
            | _ -> failwith "Expecting Success"
            let f = (p1 <-| p2) stream
            match f with
            | Success (_, '1') -> ()
            | _ -> failwith "Expecting Success"
        
        [<Fact>]
        let ``Test the sequence parser`` () =
            let p:Parser<unit, char> = 
                fun stream -> 
                    match stream.Next() with
                    | ValueSome(Rune '1', s) -> Success (s, '1')
                    | _ -> Failure (stream)
            let stream = TextStream.Create((), "1111")
            let f = sequence [p; p; p; p] stream
            match f with
            | Success (_, ['1'; '1'; '1'; '1']) -> ()
            | _ -> failwith "Expecting Success"
        
        [<Fact>]
        let ``Test the sequence2 parser`` () =
            let p:Parser<unit, char> = 
                fun stream -> 
                    match stream.Next() with
                    | ValueSome(Rune '1', s) -> Success (s, '1')
                    | _ -> Failure (stream)
            let stream = TextStream.Create((), "11")
            let f = sequence2 p p stream
            match f with
            | Success (_, ('1', '1')) -> ()
            | _ -> failwith "Expecting Success"

        [<Fact>]
        let ``Test the sequence3 parser`` () =
            let p:Parser<unit, char> = 
                fun stream -> 
                    match stream.Next() with
                    | ValueSome(Rune '1', s) -> Success (s, '1')
                    | _ -> Failure (stream)
            let stream = TextStream.Create((), "111")
            let f = sequence3 p p p stream
            match f with
            | Success (_, ('1', '1', '1')) -> ()
            | _ -> failwith "Expecting Success"

        [<Fact>]
        let ``Test the sequence4 parser`` () =
            let p:Parser<unit, char> = 
                fun stream -> 
                    match stream.Next() with
                    | ValueSome(Rune '1', s) -> Success (s, '1')
                    | _ -> Failure (stream)
            let stream = TextStream.Create((), "1111")
            let f = sequence4 p p p p stream
            match f with
            | Success (_, ('1', '1', '1', '1')) -> ()
            | _ -> failwith "Expecting Success"
            
        [<Fact>]
        let ``failWithMessage always results in Failure`` () =
            let f = "failure test"
            let p:Parser<unit, unit> =
                parse {
                    return! failWithMessage f
                }
            let r = tryRun p "" ()
            match r with
            | RunFailure(_, error, _) when error.Message.Contains f -> ()
            | _ -> failwith "Error"

        type TestState = { count: int; name: string }

        [<Fact>]
        let ``setState always results in the correct state`` () =            
            let p:Parser<TestState, unit> =
                parse {
                    do! setState { count = 1; name = "A" }
                    return ()
                }
            let r = tryRun p "" { count = 0; name = "Z" }
            match r with
            | RunSuccess({ count = 1; name = "A" }, _, _) -> ()
            | _ -> failwith "Error"

        [<Fact>]
        let ``transformState always results in the correct state`` () = 
            let p:Parser<TestState, unit> =
                parse {
                    do! transformState (fun o -> { o with count = o.count + 1 })
                    return ()
                }
            let r = tryRun p "" { count = 0; name = "A" }
            match r with
            | RunSuccess({ count = 1; name = "A" }, _, _) -> ()
            | _ -> failwith "Error"

        [<Fact>]
        let ``optional is always success`` () = 
            let p:Parser<unit, char option * char option * char option> =
                parse {
                    let! a = optional (parseChar '1')
                    let! b = optional (parseChar '2')
                    let! c = optional (parseChar '4')
                    return a, b, c
                }
            let r = tryRun p "14" ()
            match r with
            | RunSuccess(_, (Some '1', None, Some '4'), _) -> ()
            | _ -> failwith "Error"

        [<Fact>]
        let ``satisfy matches with various predicates`` () = 
            let p:Parser<unit, Rune * Rune * Rune> =
                parse {
                    let! a = satisfy (fun c -> c = Rune '1')
                    let! b = satisfy (Rune.IsDigit)
                    let! c = satisfy (Rune.IsLetter)
                    return a, b, c
                }
            let r = tryRun p "14A" ()
            match r with
            | RunSuccess(_, (Rune '1', Rune '4', Rune 'A'), _) -> ()
            | _ -> failwith "Error"

        [<Fact>]
        let ``peek is a success and does not change the stream`` () = 
            let p:Parser<unit, Rune option> =
                parse {
                    let! a = peek (satisfy Rune.IsDigit)
                    return a
                }
            let stream = TextStream.Create((), "1")
            let result = p stream
            match result with
            | Success (stream, Some (Rune '1')) 
                when stream.Index = 0 -> ()
            | _ -> failwith "Error"

        [<Fact>]
        let ``skipSequence is a success`` () = 
            let p:Parser<unit, Rune> = satisfy Rune.IsDigit
            let s:Parser<unit, unit> = skipSequence [p;p;p;]

            let stream = TextStream.Create((), "1234")
            let result = s stream
            match result with
            | Success (stream, ()) 
                when stream.Index = 3 -> ()
            | _ -> failwith "Error"

        [<Fact>]
        let ``skipParse is a success`` () = 
            let a:Parser<unit, Rune> = satisfy Rune.IsDigit
            let b:Parser<unit, Rune> = satisfy Rune.IsLetter
            let p:Parser<unit, Rune> = skipParse a b

            let stream = TextStream.Create((), "1A")
            let result = p stream
            match result with
            | Success (_, Rune 'A') -> ()
            | _ -> failwith "Error"

        [<Theory>]
        [<InlineData("1")>]
        [<InlineData("A")>]
        [<InlineData(" ")>]
        let ``choose is a success`` text = 
            let a:Parser<unit, Rune> = satisfy Rune.IsDigit
            let b:Parser<unit, Rune> = satisfy Rune.IsLetter
            let c:Parser<unit, Rune> = satisfy Rune.IsWhiteSpace
            let p:Parser<unit, Rune> = choose [a; b; c]

            let stream = TextStream.Create((), text)
            let result = p stream
            match result with
            | Success (_, Rune text) -> ()
            | _ -> failwith "Error"

        [<Theory>]
        [<InlineData("1")>]
        [<InlineData("A")>]
        let ``choose2 is a success`` text = 
            let a:Parser<unit, Rune> = satisfy Rune.IsDigit
            let b:Parser<unit, Rune> = satisfy Rune.IsLetter
            let p:Parser<unit, Rune> = choose2 a b

            let stream = TextStream.Create((), text)
            let result = p stream
            match result with
            | Success (_, Rune text) -> ()
            | _ -> failwith "Error"

        [<Theory>]
        [<InlineData("1")>]
        [<InlineData("A")>]
        [<InlineData(" ")>]
        let ``choose3 is a success`` text = 
            let a:Parser<unit, Rune> = satisfy Rune.IsDigit
            let b:Parser<unit, Rune> = satisfy Rune.IsLetter
            let c:Parser<unit, Rune> = satisfy Rune.IsWhiteSpace
            let p:Parser<unit, Rune> = choose3 a b c

            let stream = TextStream.Create((), text)
            let result = p stream
            match result with
            | Success (_, Rune text) -> ()
            | _ -> failwith "Error"

        [<Theory>]
        [<InlineData("1")>]
        [<InlineData("A")>]
        [<InlineData(" ")>]
        [<InlineData("X")>]
        let ``choose4 is a success`` text = 
            let a:Parser<unit, Rune> = satisfy Rune.IsDigit
            let b:Parser<unit, Rune> = satisfy Rune.IsLetter
            let c:Parser<unit, Rune> = satisfy Rune.IsWhiteSpace
            let d:Parser<unit, Rune> = satisfy (fun r -> r = Rune 'X')
            let p:Parser<unit, Rune> = choose4 a b c d

            let stream = TextStream.Create((), text)
            let result = p stream
            match result with
            | Success (_, Rune text) -> ()
            | _ -> failwith "Error"
            
        [<Theory>]
        [<InlineData("1", false)>]
        [<InlineData("A", true)>]
        let ``provideName is a pass through`` text shouldFail = 
            let p:Parser<unit, Rune> =
                parse {
                    let! a = provideName (satisfy Rune.IsDigit) "Single Digit"
                    return a
                }
            let stream = TextStream.Create((), text)
            let result = p stream
            match result with
            | Success (_, _) when not shouldFail -> ()
            | Failure (_) when shouldFail -> ()
            | _ -> failwith "Error"
            
        [<Theory>]
        [<InlineData("1", false)>]
        [<InlineData("A", true)>]
        let ``provideNameAndDescription is a pass through`` text shouldFail = 
            let p:Parser<unit, Rune> =
                parse {
                    let! a = provideNameAndDescription (satisfy Rune.IsDigit) ("Single Digit", "aaa")
                    return a
                }
            let stream = TextStream.Create((), text)
            let result = p stream
            match result with
            | Success (_, _) when not shouldFail -> ()
            | Failure (_) when shouldFail -> ()
            | _ -> failwith "Error"
            
        [<Theory>]
        [<InlineData("1", false)>]
        [<InlineData("A", true)>]
        let ``parseProduction is a pass through`` text shouldFail = 
            let p:Parser<unit, Rune> =
                parse {
                    let! a = parseProduction "Single Digit" (satisfy Rune.IsDigit) 
                    return a
                }
            let stream = TextStream.Create((), text)
            let result = p stream
            match result with
            | Success (_, _) when not shouldFail -> ()
            | Failure (_) when shouldFail -> ()
            | _ -> failwith "Error"

    end

