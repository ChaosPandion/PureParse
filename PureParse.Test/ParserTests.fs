namespace PureParse.Test

open System
open System.Text
open PureParse
open Xunit

module ParserTests = begin
        
        [<Fact>]
        let ``Test the bind function`` () =
            let p:Parser<unit, char> = 
                fun stream -> 
                    Success (stream, '1')
            let t:Transform<unit, char, int> = 
                fun c stream -> 
                    Success(stream, string c |> int)
            let stream = TextStream.Create((), "1")
            let f = bind p t stream
            match f with
            | Success (_, i) ->
                Assert.Equal(1, i)
            | Failure (_) ->
                failwith "Expecting Success"
            let f = (p >>= t) stream
            match f with
            | Success (_, i) ->
                Assert.Equal(1, i)
            | Failure (_) ->
                failwith "Expecting Success"

        [<Fact>]
        let ``Test the result function`` () = 
            let stream = TextStream.Create((), "1")
            match result<unit, int> 1 stream with
            | Success (_, 1) -> ()
            | _ ->
                failwith "Expecting Success"

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

    end

