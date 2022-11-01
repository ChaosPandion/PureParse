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

        [<Fact>]
        let ``skipWhiteSpace`` () =    
            match tryRun (skipWhiteSpace ()) "  " () with
            | RunSuccess (_, (), _) -> ()
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

        [<Fact>]
        let ``parseKeywords finds the correct keyword`` () =    
            match tryRun (parseKeywords [ "aaa"; "bbb"; "ccc" ]) "ccc" () with
            | RunSuccess (_, "ccc", _) -> ()
            | _ -> failwith "Unknown Result"
            
        [<Theory>]
        [<InlineData("0", 0)>]
        [<InlineData("2", 2)>]
        [<InlineData("200", 200)>]
        [<InlineData("1200", 1200)>]
        let ``parseInt32`` text expect =  
            match tryRun parseInt32 text () with
            | RunSuccess (_, expect, _) -> ()
            | _ -> failwithf "Unknown Result"
    end