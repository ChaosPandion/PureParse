module Tests

open System
open Xunit
open PureParse.Examples.PLisp.PLisp

[<Fact>]
let ``PList Test`` () =
    let text = "(aaa true (ccc ddd 123 (eee \"abcdefg\")))"
    let result = parseText text
    let expect = 
        PList [ 
            PName "aaa"; 
            PBool true; 
            PList [ 
                PName "ccc"; 
                PName "ddd"; 
                PNumber 123.0; 
                PList [ 
                    PName "eee"; 
                    PString "abcdefg"] ] ]
    Assert.Equal (expect, result)

[<Fact>]
let ``PList Big Test`` () =
    let text = "(aaa true (ccc ddd 123 (eee (aaa true (ccc ddd 123 (eee \"abcdefg\"))))))"
    let result = Seq.init 300 (fun _ -> parseText text) |> Seq.last
    let expect = 
        PList [ 
            PName "aaa"; 
            PBool true; 
            PList [ 
                PName "ccc"; 
                PName "ddd"; 
                PNumber 123.0; 
                PList [ 
                    PName "eee"; 
                    PList [ 
                        PName "aaa"; 
                        PBool true; 
                        PList [ 
                            PName "ccc"; 
                            PName "ddd"; 
                            PNumber 123.0; 
                            PList [ 
                                PName "eee"; 
                                PString "abcdefg"] ] ] ] ] ]
    Assert.Equal (expect, result)

[<Fact>]
let ``Parse Single Name`` () =
    let text = "(aaa)"
    let result = parseText text
    let expect = PList [ PName "aaa"; ]
    Assert.Equal (expect, result)

[<Fact>]
let ``Parse Bool`` () =
    let text = "(true false)"
    let result = parseText text
    let expect = PList [ PBool true; PBool false; ]
    Assert.Equal (expect, result)

[<Fact>]
let ``Parse Number`` () =
    let text = "(123 123.123)"
    let result = parseText text
    let expect = PList [ PNumber 123.0; PNumber 123.123; ]
    Assert.Equal (expect, result)

[<Fact>]
let ``Parse String`` () =
    let text = "(\"aaa\" \"\\r\")"
    let result = parseText text
    let expect = PList [ PString "aaa"; PString "\r"; ]
    Assert.Equal (expect, result)