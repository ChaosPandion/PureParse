namespace PureParse.Test

module ListParserTests =

    open System
    open Xunit
    open PureParse
    
    let pDigits:Parser<unit, char> = map (parseAnyOf (RuneCharSeq "0123456789")) (fun r -> char r.Value)
    let pOne:Parser<unit, char> = parseChar '1'
    let pComma:Parser<unit, unit> = skipChar ','

    [<Fact>]
    let ``parseList - Many - digits`` () =        
        let p = parseList pDigits (Many 0)
        let result = run p "1234567" ()
        Assert.Equal(['1'..'7'], result:>char seq)

    [<Fact>]
    let ``parseList - Until - digits`` () =        
        let p = parseList pDigits (Until pComma)
        let result = run p "1234567," ()
        Assert.Equal(['1'..'7'], result:>char seq)

    [<Fact>]
    let ``parseList - Sep - digits`` () =        
        let p = parseList pDigits (Sep (pComma, false, 0))
        let result = run p "1,2,3,4,5,6,7" ()
        Assert.Equal(['1'..'7'], result:>char seq)

    [<Theory>]
    [<InlineData("1111", 4, 0, false)>]
    [<InlineData("111", 3, 0, false)>]
    [<InlineData("11", 2, 0, false)>]
    [<InlineData("1", 1, 0, false)>]
    [<InlineData("1111", 4, 1, false)>]
    [<InlineData("111", 3, 1, false)>]
    [<InlineData("11", 2, 1, false)>]
    [<InlineData("1", 1, 1, false)>]
    [<InlineData("", 1, 1, true)>]
    [<InlineData("1111", 4, 2, false)>]
    [<InlineData("111", 3, 2, false)>]
    [<InlineData("11", 2, 2, false)>]
    [<InlineData("1", 1, 2, true)>]
    let ``parseList - Many`` text length minElements shouldThrow =        
        let p = parseList pOne (Many minElements)
        if shouldThrow then
            Assert.Throws<ParseError>(Action(fun () -> run p text () |> ignore)) |> ignore
        else
            let result = run p text ()
            Assert.Equal(length, result.Length)

    [<Fact>]
    let ``parseList - Until`` () =        
        let p = parseList pOne (Until pComma)
        let result = run p "1111," ()
        let expect = ['1';'1';'1';'1']
        Assert.Equal (expect,  result:>char seq)

    [<Fact>]
    let ``parseList - Until - Throws`` () =        
        let p = parseList pOne (Until pComma)
        Assert.Throws<ParseError>(Action(fun () -> run p "111111" () |> ignore)) |> ignore

    [<Theory>]
    [<InlineData("1,1,1,1", 4, true, 0, false)>]
    [<InlineData("1,1,1,1", 4, false, 0, false)>]
    [<InlineData("1,1,1,1,", 4, false, 0, true)>]
    [<InlineData("1,1,1,1", 4, true, 1, false)>]
    [<InlineData("1,1,1,1", 4, false, 1, false)>]
    [<InlineData("1,1,1,1,", 4, false, 1, true)>]
    [<InlineData("", 0, false, 1, true)>]
    [<InlineData("1,1,1,1", 4, true, 2, false)>]
    [<InlineData("1,1,1,1", 4, false, 2, false)>]
    [<InlineData("1,1,1,1,", 4, false, 2, true)>]
    [<InlineData("1", 0, false, 2, true)>]
    let ``parseList - Sep`` text length allowTrailingSep minElements shouldThrow =        
        let p = parseList pOne (Sep(pComma, allowTrailingSep, minElements))
        if shouldThrow then
            Assert.Throws<ParseError>(Action(fun () -> run p text () |> ignore)) |> ignore
        else
            let result = run p text ()
            Assert.Equal(length, result.Length)