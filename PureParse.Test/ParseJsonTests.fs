module ParseJsonTests

open System
open Xunit
open PureParse.Examples.Json

[<Fact>]
let ``JSON Boolean Test - True`` () =
    let text = """true"""
    let result = parseText text
    let expect = JsonBoolean true
    Assert.Equal (expect, result)

[<Fact>]
let ``JSON Boolean Test - False`` () =
    let text = """false"""
    let result = parseText text
    let expect = JsonBoolean false
    Assert.Equal (expect, result)

[<Fact>]
let ``JSON Null Test`` () =
    let text = """null"""
    let result = parseText text
    let expect = JsonNull
    Assert.Equal (expect, result)

[<Fact>]
let ``JSON Boolean Test - True with white space`` () =
    let text = """  true  """
    let result = parseText text
    let expect = JsonBoolean true
    Assert.Equal (expect, result)

[<Fact>]
let ``JSON Boolean Test - False with white space`` () =
    let text = """  false   """
    let result = parseText text
    let expect = JsonBoolean false
    Assert.Equal (expect, result)

[<Fact>]
let ``JSON Null Test with white space`` () =
    let text = """     null  """
    let result = parseText text
    let expect = JsonNull
    Assert.Equal (expect, result)

[<Fact>]
let ``JSON Integer Test`` () =
    let text = """1234"""
    let result = parseText text
    let expect = JsonNumber 1234.0
    Assert.Equal (expect, result)

[<Fact>]
let ``JSON Integer Test with white space`` () =
    let text = """  1234  """
    let result = parseText text
    let expect = JsonNumber 1234.0
    Assert.Equal (expect, result)

[<Fact>]
let ``JSON Real Test`` () =
    let text = """1.23"""
    let result = parseText text
    let expect = JsonNumber 1.23
    Assert.Equal (expect, result)

[<Fact>]
let ``JSON Real Test with white space`` () =
    let text = """  1.23  """
    let result = parseText text
    let expect = JsonNumber 1.23
    Assert.Equal (expect, result)


[<Fact>]
let ``JSON Negative Real Test`` () =
    let text = """ -1.23 """
    let result = parseText text
    let expect = JsonNumber -1.23
    Assert.Equal (expect, result)

[<Fact>]
let ``JSON Real Test - Full Number`` () =
    let text = """  -1.23e+1  """
    let result = parseText text
    let expect = JsonNumber -1.23e+1
    Assert.Equal (expect, result)

[<Fact>]
let ``JSON String Test`` () =
    let text = "\"aaa\""
    let result = parseText text
    let expect = JsonString "aaa"
    Assert.Equal (expect, result)

[<Fact>]
let ``JSON String Test with white space`` () =
    let text = """ "aaa"  """
    let result = parseText text
    let expect = JsonString "aaa"
    Assert.Equal (expect, result)

[<Fact>]
let ``JSON String Test with escape characters`` () =
    let text = "\" \\\" \\\\ \\/ \\b \\f \\n \\r \\t \\u00aa \""
    let result = parseText text
    let expect = JsonString " \" \\ / \b \f \n \r \t \u00aa "
    Assert.Equal (expect, result)

[<Fact>]
let ``JSON Object Test`` () =
    let text = """ { "aaa": 123, "bbb": true } """
    let result = parseText text
    let expect = 
        JsonObject ([ ("aaa", JsonNumber 123.0); ("bbb", JsonBoolean true)] |> Map.ofList)
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