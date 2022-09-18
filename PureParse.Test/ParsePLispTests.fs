namespace PureParse.Test

module ParsePLispTests =

    open System
    open Xunit
    open PureParse.Examples.PLisp
    
    [<Theory>]
    [<InlineData("()")>]
    [<InlineData("( )")>]
    [<InlineData(" ( ) ")>]
    [<InlineData("\n( ) ")>]
    [<InlineData("\n()\n")>]
    [<InlineData("\n(\n\t)\n")>]
    [<InlineData("\n(\n\t)\r ")>]
    [<InlineData("\r\n(\r\n\t)\r\n")>]
    [<InlineData("\r\n(\t)")>]
    [<InlineData("(\t)\r\n")>]
    let ``Parse an empty list with varying amount of white space.`` text =
        let result = parseText text
        let expect =  PList []
        Assert.Equal (expect, result)
    
    [<Theory>]
    [<InlineData("(true)", true)>]
    [<InlineData("(false)", false)>]
    [<InlineData(" (true)", true)>]
    [<InlineData(" (false)", false)>]
    [<InlineData("( true)", true)>]
    [<InlineData("( false)", false)>]
    [<InlineData("(true )", true)>]
    [<InlineData("(false )", false)>]
    [<InlineData("(true\r)", true)>]
    [<InlineData("(false\r)", false)>]
    let ``Parse a list with a single boolean.`` text expect =
        let result = parseText text
        let expect =  PList [ PBool expect ]
        Assert.Equal (expect, result)

    [<Theory>]
    [<InlineData("(true false)", true, false)>]
    [<InlineData("(false true)", false, true)>]
    [<InlineData("(true true)", true, true)>]
    [<InlineData("(false false)", false, false)>]

    [<InlineData(" ( true false)", true, false)>]
    [<InlineData(" ( false true)", false, true)>]
    [<InlineData(" ( true true)", true, true)>]
    [<InlineData(" ( false false)", false, false)>]
    
    [<InlineData(" (true false )", true, false)>]
    [<InlineData(" (false true )", false, true)>]
    [<InlineData(" (true true )", true, true)>]
    [<InlineData(" (false false )", false, false)>]

    [<InlineData("( true false)", true, false)>]
    [<InlineData("( false true)", false, true)>]
    [<InlineData("( true true)", true, true)>]
    [<InlineData("( false false)", false, false)>]
    
    [<InlineData("(true false )", true, false)>]
    [<InlineData("(false true )", false, true)>]
    [<InlineData("(true true )", true, true)>]
    [<InlineData("(false false )", false, false)>]
    let ``Parse a list with a two booleans.`` text expect1 expect2 =
        let result = parseText text
        let expect =  PList [ PBool expect1; PBool expect2 ]
        Assert.Equal (expect, result)
    
    [<Theory>]
    [<InlineData("(aaa)", "aaa")>]
    [<InlineData("( aaa)", "aaa")>]
    [<InlineData(" (aaa)", "aaa")>]
    [<InlineData(" ( aaa)", "aaa")>]
    [<InlineData("(aaa )", "aaa")>]
    [<InlineData("(aaa) ", "aaa")>]
    [<InlineData("(aaa ) ", "aaa")>]
    let ``Parse a list with a single name.`` text expect =
        let result = parseText text
        let expect =  PList [ PName expect ]
        Assert.Equal (expect, result)

    [<Theory>]
    [<InlineData("(aaa bbb)", "aaa", "bbb")>]
    [<InlineData("( aaa bbb)", "aaa", "bbb")>]
    [<InlineData(" (aaa bbb)", "aaa", "bbb")>]
    [<InlineData(" ( aaa bbb)", "aaa", "bbb")>]
    [<InlineData("(aaa bbb )", "aaa", "bbb")>]
    [<InlineData("(aaa bbb) ", "aaa", "bbb")>]
    [<InlineData("(aaa bbb ) ", "aaa", "bbb")>]
    let ``Parse a list with a two names.`` text expect1 expect2 =
        let result = parseText text
        let expect =  PList [ PName expect1; PName expect2 ]
        Assert.Equal (expect, result)
    
    [<Theory>]
    [<InlineData("(aaa true)", "aaa", true)>]
    [<InlineData("( aaa true)", "aaa", true)>]
    [<InlineData(" (aaa true)", "aaa", true)>]
    [<InlineData(" ( aaa true)", "aaa", true)>]
    [<InlineData("(aaa true )", "aaa", true)>]
    [<InlineData("(aaa true) ", "aaa", true)>]
    let ``Parse a list with a single name and a single boolean.`` text expectName expectBool =
        let result = parseText text
        let expect =  PList [ PName expectName; PBool expectBool ]
        Assert.Equal (expect, result)

    [<Theory>]
    [<InlineData(0.0, "(0)")>]
    [<InlineData(1.0, "(1)")>]
    [<InlineData(10.0, "(10)")>]
    [<InlineData(100.0, "(100)")>]
    [<InlineData(100000000.0, "(100000000)")>]
    [<InlineData(1.1, "(1.1)")>]
    [<InlineData(1.12, "(1.12)")>]
    [<InlineData(1.123, "(1.123)")>]
    [<InlineData(-1.1, "( -1.1)")>]
    [<InlineData(-1.12, "( -1.12)")>]
    [<InlineData(-1.123, "( -1.123 )")>]
    [<InlineData(1.1e1, "(1.1e1)")>]
    [<InlineData(1.12e1, "(1.12e1)")>]
    [<InlineData(1.123e1, "(1.123e1)")>]
    [<InlineData(1.1e+1, "(1.1e+1)")>]
    [<InlineData(1.12e+1, "(1.12e+1)")>]
    [<InlineData(1.123e+1, "(1.123e+1)")>]
    [<InlineData(1.1e-1, "(1.1e-1)")>]
    [<InlineData(1.12e-1, "(1.12e-1)")>]
    [<InlineData(1.123e-1, "(1.123e-1)")>]
    [<InlineData(1023.999e+99, "(1023.999e+99)")>]
    [<InlineData(-1023.999e+99, "(-1023.999e+99)")>]
    [<InlineData(1.2e-99, "(1.2e-99)")>]
    [<InlineData(1.2E-99, "(1.2E-99)")>]
    let ``Parse a list with a single number.`` expect text =
        let result = parseText text
        match result with
        | PList [PNumber n] when Math.Abs(expect - n) = 0 ->
            Assert.Equal(expect, n)
        | PList [PNumber n] when Math.Abs(expect - n) > 0.00000001 ->
            Assert.Equal(expect, n)
        | _ -> () // Success


    [<Theory>]
    [<InlineData("(\"a string\")", "a string")>]
    [<InlineData(" (\"a string\")", "a string")>]
    [<InlineData("(\"a string\") ", "a string")>]
    [<InlineData(" (\"a string\") ", "a string")>]
    [<InlineData(" ( \"a string\") ", "a string")>]
    [<InlineData(" ( \"a string\" ) ", "a string")>]
    [<InlineData(" (\"a string\" ) ", "a string")>]
    [<InlineData("(\"aaa \\t\")", "aaa \t")>]
    [<InlineData("(\"aaa \\r\")", "aaa \r")>]
    [<InlineData("(\"aaa \\n\")", "aaa \n")>]
    [<InlineData("(\"aaa \\\"\")", "aaa \"")>]
    [<InlineData("(\"aaa \\\\\")", "aaa \\")>]
    let ``Parse a list a single string.`` text expect =
        let result = parseText text
        let expect =  PList [ PString expect; ]
        Assert.Equal (expect, result)

    [<Theory>]
    [<InlineData("(!)", "!")>]
    [<InlineData("(@)", "@")>]
    [<InlineData("(#)", "#")>]
    [<InlineData("($)", "$")>]
    [<InlineData("(^)", "^")>]
    [<InlineData("(&)", "&")>]
    [<InlineData("(*)", "*")>]
    [<InlineData("(-)", "-")>]
    [<InlineData("(+)", "+")>]
    [<InlineData("(=)", "=")>]
    [<InlineData("(?)", "?")>]
    [<InlineData("(/)", "/")>]
    [<InlineData("(<)", "<")>]
    [<InlineData("(>)", ">")>]
    [<InlineData("(~)", "~")>]
    [<InlineData("(`)", "`")>]
    [<InlineData("(_)", "_")>]
    [<InlineData("([)", "[")>]
    [<InlineData("(])", "]")>]
    [<InlineData("(;)", ";")>]
    [<InlineData("(:)", ":")>]
    [<InlineData("(alert!)", "alert!")>]
    [<InlineData("(++)", "++")>]
    [<InlineData("(!!)", "!!")>]
    [<InlineData("(+append!)", "+append!")>]
    let ``Parse a list a single special name.`` text expect =
        let result = parseText text
        let expect =  PList [ PName expect; ]
        Assert.Equal (expect, result)

    [<Fact>]
    let ``Parse a list a bunch of values`` () =
        let result = parseText "(1 true test \"aaa\" (1 2 3 4 5))"
        let expect =  PList [ 
                        PNumber 1.0; 
                        PBool true; 
                        PName "test"; 
                        PString "aaa"; 
                        PList [ 
                            PNumber 1;
                            PNumber 2; 
                            PNumber 3; 
                            PNumber 4; 
                            PNumber 5 ] ]
        Assert.Equal (expect, result)
    
    [<Fact>]
    let ``Parse a big list of values`` () =
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