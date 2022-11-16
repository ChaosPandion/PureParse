namespace PureParse.Test

open System
open System.Text
open PureParse
open Xunit

module TextParserTests = 
    begin        
        
        [<Theory>]
        [<InlineData('0', '9', "0", '0', true)>]
        [<InlineData('0', '9', "A", 'A', false)>]
        [<InlineData('A', 'F', "A", 'A', true)>]
        [<InlineData('A', 'F', "1", '1', false)>]
        let ``charRange tests`` minChar maxChar input expect success =
            let test p n =         
                match tryRun p input () with
                | RunSuccess (_, x, _) when success -> Assert.Equal(char expect, x)
                | RunSuccess (_, x, _) when not success -> failwithf "Expecting Failure for %s" n
                | RunFailure (_, _, _) when success -> failwithf "Expecting Success for %s" n
                | RunFailure (_, _, _) when not success -> ()
                | _ -> failwithf "Unknown Result for %s" n
            test (charRange minChar maxChar) "charRange"

        [<Theory>]
        [<InlineData('0', '9', "0", '0', true)>]
        [<InlineData('0', '9', "A", 'A', false)>]
        [<InlineData('A', 'F', "A", 'A', true)>]
        [<InlineData('A', 'F', "1", '1', false)>]
        let ``runeRange tests.`` minChar maxChar input expect success =
            let test p n =         
                match tryRun p input () with
                | RunSuccess (_, x, _) when success -> Assert.Equal(Rune(char expect), x)
                | RunSuccess (_, x, _) when not success -> failwithf "Expecting Failure for %s" n
                | RunFailure (_, _, _) when success -> failwithf "Expecting Success for %s" n
                | RunFailure (_, _, _) when not success -> ()
                | _ -> failwithf "Unknown Result for %s" n
            test (runeRange (Rune (char minChar)) (Rune (char maxChar))) "runeRange"
            
        [<Theory>]
        [<InlineData('0', "0", '0', true)>]
        [<InlineData('0', "1", '1', false)>]
        let ``parseRune tests.`` c input expect success =        
            match tryRun (parseRune (Rune (char c))) input () with
            | RunSuccess (_, x, _) when success -> Assert.Equal(Rune(char expect), x)
            | RunSuccess (_, _, _) when not success -> failwith "Expecting Failure"
            | RunFailure (_, _, _) when success -> failwithf "Expecting Success"
            | RunFailure (_, _, _) when not success -> ()
            | _ -> failwithf "Unknown Result"

        [<Theory>]
        [<InlineData('0', "0", '0', true)>]
        [<InlineData('0', "1", '1', false)>]
        let ``parseChar tests.`` c input expect success =        
            match tryRun (parseChar (char c)) input () with
            | RunSuccess (_, x, _) when success -> Assert.Equal(char expect, x)
            | RunSuccess (_, _, _) when not success -> failwith "Expecting Failure"
            | RunFailure (_, _, _) when success -> failwithf "Expecting Success"
            | RunFailure (_, _, _) when not success -> ()
            | _ -> failwithf "Unknown Result"

        [<Theory>]
        [<InlineData("000", "000", "000", true)>]
        [<InlineData("000", "111", "111", false)>]
        let ``parseString tests.`` s input expect success =        
            match tryRun (parseString s) input () with
            | RunSuccess (_, x, _) when success -> Assert.Equal(expect, x)
            | RunSuccess (_, _, _) when not success -> failwith "Expecting Failure"
            | RunFailure (_, _, _) when success -> failwithf "Expecting Success"
            | RunFailure (_, _, _) when not success -> ()
            | _ -> failwithf "Unknown Result"

        [<Theory>]
        [<InlineData("   ", "   ", true)>]
        [<InlineData("\n\n\n", "\n\n\n", true)>]
        [<InlineData("111", "111", false)>]
        let ``parseWhiteSpace tests.`` input expect success =        
            match tryRun (parseWhiteSpace ()) input () with
            | RunSuccess (_, x, _) when success -> Assert.Equal(expect, x)
            | RunSuccess (_, _, _) when not success -> failwith "Expecting Failure"
            | RunFailure (_, _, _) when success -> failwithf "Expecting Success"
            | RunFailure (_, _, _) when not success -> ()
            | _ -> failwithf "Unknown Result"

        [<Fact>]
        let ``skipRune`` () =    
            match tryRun (skipRune (Rune 'A')) "A" () with
            | RunSuccess (_, (), _) -> ()
            | _ -> failwith "Unknown Result"

        [<Fact>]
        let ``skipChar`` () =    
            match tryRun (skipChar 'A') "A" () with
            | RunSuccess (_, (), _) -> ()
            | _ -> failwith "Unknown Result"

        [<Fact>]
        let ``skipString`` () =    
            match tryRun (skipString "AAAA") "AAAA" () with
            | RunSuccess (_, (), _) -> ()
            | _ -> failwith "Unknown Result"
                        
        [<Theory>]
        [<InlineData(" ", 1)>]
        [<InlineData("  ", 2)>]
        [<InlineData("   ", 3)>]
        [<InlineData("\n\n", 2)>]
        [<InlineData("\t\t", 2)>]
        [<InlineData("\t \t \n", 5)>]
        let ``skipWhiteSpace skips passed every space.`` text expectedIndex =   
            let stream = TextStream.Create ((), text)
            match skipWhiteSpace<unit> () stream with
            | Success (stream, _) ->
                Assert.Equal(expectedIndex, stream.Index)
            | _ -> failwith "Unknown Result"

        [<Theory>]
        [<InlineData(true, "ABC", "B", 'B', true)>]
        [<InlineData(true, "ABC", "1", '1', false)>]
        [<InlineData(false, "ABC", "1", '1', true)>]
        [<InlineData(false, "ABC", "A", 'A', false)>]
        let ``parseRuneBySet`` contains (setString:string) input (expect:char) success =  
            let set = setString |> Seq.map Rune |> Set.ofSeq
            let p = parseRuneBySet<unit> set contains
            match tryRun p input () with
            | RunSuccess (_, Rune expect, _) when success -> ()
            | RunSuccess (_, _, _) when not success -> failwith "Expecting Failure"
            | RunFailure (_, _, _) when success -> failwithf "Expecting Success"
            | RunFailure (_, _, _) when not success -> ()
            | _ -> failwithf "Unknown Result"

        [<Fact>]
        let ``parseAnyOf(set)`` () =    
            match tryRun (parseAnyOf (RuneSet (Set.ofList[Rune 'a'; Rune 'b']))) "a" () with
            | RunSuccess (_, Rune 'a', _) -> ()
            | _ -> failwith "Unknown Result"

        [<Fact>]
        let ``parseAnyOf(runeSeq)`` () =    
            match tryRun (parseAnyOf (RuneSeq (Seq.ofList[Rune 'a'; Rune 'b']))) "a" () with
            | RunSuccess (_, Rune 'a', _) -> ()
            | _ -> failwith "Unknown Result"

        [<Fact>]
        let ``parseAnyOf(runeString)`` () =    
            match tryRun (parseAnyOf (RuneString "ab")) "a" () with
            | RunSuccess (_, Rune 'a', _) -> ()
            | _ -> failwith "Unknown Result"

        [<Fact>]
        let ``parseAnyOf(runeCharSeq)`` () =    
            match tryRun (parseAnyOf (RuneCharSeq "ab")) "a" () with
            | RunSuccess (_, Rune 'a', _) -> ()
            | _ -> failwith "Unknown Result"

        [<Fact>]
        let ``parseAnyOf(runeCharArray)`` () =    
            match tryRun (parseAnyOf (RuneCharArray ("ab" |> Seq.toArray))) "a" () with
            | RunSuccess (_, Rune 'a', _) -> ()
            | _ -> failwith "Unknown Result"
                        
        [<Theory>]
        [<InlineData("0", 0)>]
        [<InlineData("2", 2)>]
        [<InlineData("200", 200)>]
        [<InlineData("1200", 1200)>]
        [<InlineData("-0", 0)>]
        [<InlineData("-2", -2)>]
        [<InlineData("-200", -200)>]
        [<InlineData("-1200", -1200)>]
        let ``parseInt32`` text expect =  
            match tryRun (parseInt32 ()) text () with
            | RunSuccess (_, e, _) when expect = e -> ()
            | _ -> failwithf "Unknown Result"

        [<Fact>]
        [<Trait("Method", "parseAnyString")>]
        let ``parseAnyString matches the longest word`` () = 
            let p = parseAnyString<unit> [ "test"; "testing" ]
            match tryRun p "testing" () with
            | RunSuccess (_, "testing", _) -> ()
            | RunSuccess (_, a, _) -> failwith $"Unknown Result: {a}"
            | _ -> failwith "Unknown Result"

        [<Fact>]
        [<Trait("Method", "parseAnyString")>]
        let ``parseAnyString matches one word`` () = 
            let p = parseAnyString<unit> [ "test" ]
            match tryRun p "test" () with
            | RunSuccess (_, "test", _) -> ()
            | RunSuccess (_, a, _) -> failwith $"Unknown Result: {a}"
            | _ -> failwith "Unknown Result"

        [<Fact>]
        [<Trait("Method", "parseAnyString")>]
        let ``parseAnyString fails to find a match`` () = 
            let p = parseAnyString<unit> [ "test"; "testing" ]
            match tryRun p "te" () with
            | RunSuccess (_, a, _) -> failwith $"Unknown Result: {a}"
            | RunFailure(_, _, _) -> ()
            
        [<Fact>]
        [<Trait("Method", "parseAnyString")>]
        let ``parseAnyString matches the correct word`` () =  
            let words = [ "true"; "false"; "null"; "nullish"; "nullite"; "nullate"; "trukish"; "then"; "thenling" ]
            let p = parseAnyString<unit> words
            match tryRun p "nullate" () with
            | RunSuccess (_, "nullate", _) -> ()
            | RunSuccess (_, a, _) -> failwith $"Unknown Result: {a}"
            | _ -> failwith "Unknown Result"
            match tryRun p "trukish" () with
            | RunSuccess (_, "trukish", _) -> ()
            | RunSuccess (_, a, _) -> failwith $"Unknown Result: {a}"
            | _ -> failwith "Unknown Result"
            match tryRun p "true" () with
            | RunSuccess (_, "true", _) -> ()
            | RunSuccess (_, a, _) -> failwith $"Unknown Result: {a}"
            | _ -> failwith "Unknown Result"
            match tryRun p "null" () with
            | RunSuccess (_, "null", _) -> ()
            | RunSuccess (_, a, _) -> failwith $"Unknown Result: {a}"
            | _ -> failwith "Unknown Result"

        [<Fact>]
        [<Trait("Method", "parseAnyString")>]
        let ``parseAnyString matches the correct word when they all start with the same prefix`` () =  
            let words = [ "t"; "ta"; "tb"; "tc"; "td"; "te"; "tf"; "tg"; "th"; "ti"; ]
            let p = parseAnyString<unit> words
            match tryRun p "ti" () with
            | RunSuccess (_, "ti", _) -> ()
            | RunSuccess (_, a, _) -> failwith $"Unknown Result: {a}"
            | _ -> failwith "Unknown Result"

        [<Fact>]
        [<Trait("Method", "parseAnyString")>]
        let ``parseAnyString matches when there is not prefix`` () =  
            let words = [ "AAA"; "BBB"; "CCC"; "DDD"; "EEE"; "FFF"; "GGG"; "HHH"; "III"; ]
            let p = parseAnyString<unit> words
            match tryRun p "FFF" () with
            | RunSuccess (_, "FFF", _) -> ()
            | RunSuccess (_, a, _) -> failwith $"Unknown Result: {a}"
            | _ -> failwith "Unknown Result"

        [<Fact>]
        [<Trait("Method", "parseAnyString")>]
        let ``parseAnyString matches when there is only one prefix`` () =  
            let words = [ "AAA"; "AAAA"; "BBB"; "CCC"; "DDD"; "EEE"; "FFF"; "GGG"; "HHH"; "III"; ]
            let p = parseAnyString<unit> words
            match tryRun p "FFF" () with
            | RunSuccess (_, "FFF", _) -> ()
            | RunSuccess (_, a, _) -> failwith $"Unknown Result: {a}"
            | _ -> failwith "Unknown Result"

        [<Fact>]
        [<Trait("Method", "parseAnyString")>]
        let ``parseAnyString fails when the list has duplicates`` () =  
            Assert.ThrowsAny(fun () -> parseAnyString<unit> [ "AAA"; "AAA"; ] |> ignore) |> ignore

        [<Fact>]
        [<Trait("Method", "parseAnyString")>]
        let ``parseAnyString fails when the list is empty`` () =  
            Assert.ThrowsAny(fun () -> parseAnyString<unit> [ ] |> ignore) |> ignore

        [<Trait("Method", "parseAnyString")>]
        let ``parseAnyString fails when the list has an empty string`` () =  
            Assert.ThrowsAny(fun () -> parseAnyString<unit> [ "AAA"; "" ] |> ignore) |> ignore

        [<Trait("Method", "parseAnyString")>]
        let ``parseAnyString fails when the list has a null string`` () =  
            Assert.ThrowsAny(fun () -> parseAnyString<unit> [ "AAA"; null ] |> ignore) |> ignore
            
        [<Fact>]
        let ``parseCharString creates an entire string given a simple parser.`` () = 
            let p = parseRune (Rune 'A')
            let p = parseCharString p
            match tryRun p "AAAAAAAAAAAAAAA" () with
            | RunSuccess (_, "AAAAAAAAAAAAAAA", _) -> ()
            | RunSuccess (_, a, _) -> failwith $"Unknown Result: {a}"
            | _ -> failwith "Unknown Result"
    end