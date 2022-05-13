namespace PureParse.Test

module ParseJsonTests =

    open System
    open Xunit
    open PureParse.Examples.Json

    [<Fact>]
    let ``JSON Object Test`` () =
        let text = """ { "aaa": 123, "bbb": true } """
        let result = parseText text
        let expect = 
            JsonObject ([ ("aaa", JsonNumber 123.0); ("bbb", JsonBoolean true)] |> Map.ofList)
        Assert.Equal (expect, result)

    [<Fact>]
    let ``JSON Expanded ObjectTest`` () =
        let text = """{ 
                            "aaa": 123, 
                            "bbb": true, 
                            "ccc" : {
                                "ddd": "123",
                                "eee" : null,
                                "fff": [
                                    123,
                                    true,
                                    "ggg"
                                ]
                            }
                      }"""
        let result = parseText text
        let expect = 
            JsonObject ([ 
                ("aaa", JsonNumber 123.0); 
                ("bbb", JsonBoolean true);
                ("ccc", JsonObject ([
                        ("ddd", JsonString "123"); 
                        ("eee", JsonNull); 
                        ("fff", JsonArray ([
                            JsonNumber 123.0
                            JsonBoolean true; 
                            JsonString "ggg";]))] |> Map.ofList))] |> Map.ofList)
        Assert.Equal (expect, result)

    [<Fact>]
    let ``JSON Array Test with simple values`` () =
        let text = """ [  "aaa", true, false, null, 1.2 ] """
        let result = parseText text
        let expect = 
            JsonArray ([ JsonString "aaa"; JsonBoolean true; JsonBoolean false; JsonNull; JsonNumber 1.2; ])
        Assert.Equal (expect, result)

    [<Fact>]
    let ``JSON Array Test with simple values - recursive`` () =
        let text = """ [  "aaa", true, false, null, 1.2, [  "aaa", true, false, null, 1.2, { "aaa": 123, "bbb": true } ] ] """
        let result = parseText text
        let expect = 
            JsonArray ([ JsonString "aaa"; JsonBoolean true; JsonBoolean false; JsonNull; JsonNumber 1.2; 
                JsonArray ([ JsonString "aaa"; JsonBoolean true; JsonBoolean false; JsonNull; JsonNumber 1.2; 
                    JsonObject ([ ("aaa", JsonNumber 123.0); ("bbb", JsonBoolean true)] |> Map.ofList) ]) ])
        Assert.Equal (expect, result)

    [<Fact>]
    let ``JSON Parse An Array of Objects`` () =
        let text = """ [ { "name": "c#", "family": "c" }, { "name": "f#", "family": "ml" } ] """
        let result = parseText text
        let expect = 
            JsonArray ([ JsonObject ([ ("name", JsonString "c#"); ("family", JsonString "c")] |> Map.ofList);
                            JsonObject ([ ("name", JsonString "f#"); ("family", JsonString "ml")] |> Map.ofList)])
        Assert.Equal (expect, result)

    [<Theory>]
    [<InlineData("[]")>]
    [<InlineData("[ ]")>]
    [<InlineData(" [ ] ")>]
    [<InlineData("\n[ ] ")>]
    [<InlineData("\n[]\n")>]
    [<InlineData("\n[\n\t]\n")>]
    [<InlineData("\n[\n\t]\r ")>]
    let ``JSON Parse An Empty Array`` text =
        let result = parseText text
        let expect =  JsonArray []
        Assert.Equal (expect, result)

    [<Theory>]
    [<InlineData("{}")>]
    [<InlineData("{ }")>]
    [<InlineData(" { } ")>]
    [<InlineData("\n{ } ")>]
    [<InlineData("\n{}\n")>]
    [<InlineData("\n{\n\t}\n")>]
    [<InlineData("\n{\n\t}\r ")>]
    let ``JSON Parse An Empty Object`` text =
        let result = parseText text
        let expect =  JsonObject Map.empty
        Assert.Equal (expect, result)

    [<Theory>]
    [<InlineData(true, "true")>]
    [<InlineData(true, " true ")>]
    [<InlineData(true, " true\r\n ")>]
    [<InlineData(true, "\r\n\ttrue\r\n ")>]
    [<InlineData(false, "false")>]
    [<InlineData(false, " false ")>]
    [<InlineData(false, " false\r\n ")>]
    [<InlineData(false, "\r\n\tfalse\r\n ")>]
    let ``JSON Parse A Boolean`` expect text =
        let result = parseText text
        let expect =  JsonBoolean expect
        Assert.Equal (expect, result)

    [<Theory>]
    [<InlineData("null")>]
    [<InlineData(" null ")>]
    [<InlineData(" null\r\n ")>]
    [<InlineData("\r\n\tnull\r\n ")>]
    let ``JSON Parse A Null`` text =
        let result = parseText text
        let expect =  JsonNull
        Assert.Equal (expect, result)

    [<Theory>]
    [<InlineData(1, "1")>]
    [<InlineData(12, "12")>]
    [<InlineData(123, "123")>]
    [<InlineData(1, " 1 ")>]
    [<InlineData(12, " 12 ")>]
    [<InlineData(123, " 123 ")>]
    [<InlineData(+1, "+1")>]
    [<InlineData(+12, "+12")>]
    [<InlineData(+123, "+123")>]
    [<InlineData(+1, " +1 ")>]
    [<InlineData(+12, " +12 ")>]
    [<InlineData(+123, " +123 ")>]
    [<InlineData(-1, "-1")>]
    [<InlineData(-12, "-12")>]
    [<InlineData(-123, "-123")>]
    [<InlineData(-1, " -1 ")>]
    [<InlineData(-12, " -12 ")>]
    [<InlineData(-123, " -123 ")>]
    [<InlineData(1.1, "1.1")>]
    [<InlineData(1.12, "1.12")>]
    [<InlineData(1.123, "1.123")>]
    [<InlineData(1.1, " 1.1 ")>]
    [<InlineData(1.12, " 1.12 ")>]
    [<InlineData(1.123, " 1.123 ")>]
    [<InlineData(+1.1, "+1.1")>]
    [<InlineData(+1.12, "+1.12")>]
    [<InlineData(+1.123, "+1.123")>]
    [<InlineData(+1.1, " +1.1 ")>]
    [<InlineData(+1.12, " +1.12 ")>]
    [<InlineData(+1.123, " +1.123 ")>]
    [<InlineData(-1.1, "-1.1")>]
    [<InlineData(-1.12, "-1.12")>]
    [<InlineData(-1.123, "-1.123")>]
    [<InlineData(-1.1, " -1.1 ")>]
    [<InlineData(-1.12, " -1.12 ")>]
    [<InlineData(-1.123, " -1.123 ")>]
    [<InlineData(1.1e1, "1.1e1")>]
    [<InlineData(1.12e1, "1.12e1")>]
    [<InlineData(1.123e1, "1.123e1")>]
    [<InlineData(1.1e1, " 1.1e1 ")>]
    [<InlineData(1.12e1, " 1.12e1 ")>]
    [<InlineData(1.123e1, " 1.123e1 ")>]
    [<InlineData(1.1e+1, "1.1e+1")>]
    [<InlineData(1.12e+1, "1.12e+1")>]
    [<InlineData(1.123e+1, "1.123e+1")>]
    [<InlineData(1.1e+1, " 1.1e+1 ")>]
    [<InlineData(1.12e+1, " 1.12e+1 ")>]
    [<InlineData(1.123e+1, " 1.123e+1 ")>]
    [<InlineData(1.1e-1, "1.1e-1")>]
    [<InlineData(1.12e-1, "1.12e-1")>]
    [<InlineData(1.123e-1, "1.123e-1")>]
    [<InlineData(1.1e-1, " 1.1e-1 ")>]
    [<InlineData(1.12e-1, " 1.12e-1 ")>]
    [<InlineData(1.123e-1, " 1.123e-1 ")>]
    [<InlineData(1023.999e+99, "1023.999e+99")>]
    [<InlineData(-1023.999e+99, "-1023.999e+99")>]
    [<InlineData(1023.999e+99, "\r\n\t 1023.999e+99\n\t\t\t\r")>]
    [<InlineData(1.2e-99, "1.2e-99")>]
    let ``JSON Parse A Number`` expect text =
        let result = parseText text
        match result with
        | JsonNumber n when Math.Abs(expect - n) = 0 ->
            Assert.Equal(expect, n)
        | JsonNumber n when Math.Abs(expect - n) > 0.00000001 ->
            Assert.Equal(expect, n)
        | JsonNumber _ -> ()
        | _ -> failwith "wrong type."


    [<Theory>]
    [<InlineData("123", "\"123\"")>]
    [<InlineData("123", " \"123\" ")>]
    [<InlineData("123", " \r\n\"123\"\t\t ")>]
    [<InlineData("■∑", "\"■∑\"")>]
    [<InlineData("■∑", " \"■∑\" ")>]
    [<InlineData("■∑", "\n\t\t\"■∑\" ")>]
    [<InlineData("\"", "\"\\\"\"")>]
    [<InlineData("\\", "\"\\\\\"")>]
    [<InlineData("/", "\"\\/\"")>]
    [<InlineData("\b", "\"\\b\"")>]
    [<InlineData("\f", "\"\\f\"")>]
    [<InlineData("\n", "\"\\n\"")>]
    [<InlineData("\r", "\"\\r\"")>]
    [<InlineData("\t", "\"\\t\"")>]
    [<InlineData("\u000A", "\"\\u000A\"")>]
    [<InlineData("\u000a", "\"\\u000a\"")>]
    [<InlineData("\u00AA", "\"\\u00AA\"")>]
    [<InlineData("\u00aa", "\"\\u00aa\"")>]
    [<InlineData("\u01AA", "\"\\u01AA\"")>]
    [<InlineData("\u01aa", "\"\\u01aa\"")>]
    [<InlineData("\u11AA", "\"\\u11AA\"")>]
    [<InlineData("\u11aa", "\"\\u11aa\"")>]
    [<InlineData("This is a complete sentence.", "\"This is a complete sentence.\"")>]
    [<InlineData("This is a complete sentence.", " \"This is a complete sentence.\" ")>]
    [<InlineData("This is a complete sentence.", "\r\n\t\"This is a complete sentence.\" ")>]
    let ``JSON Parse A String`` expect text =
        let result = parseText text
        match result with
        | JsonString s ->
            Assert.Equal(expect, s)
        | _ -> failwith "wrong type."