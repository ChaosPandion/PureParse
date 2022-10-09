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
        match tryRun p "1234567" () with
        | RunSuccess(_, result, _) ->
            Assert.Equal(['1'..'7'], result:>char seq)
        | _ -> failwith "Failed to parse list"

    [<Fact>]
    let ``parseList - Until - digits`` () =        
        let p = parseList pDigits (Until pComma)
        match tryRun p "1234567," () with
        | RunSuccess(_, result, _) ->
            Assert.Equal(['1'..'7'], result:>char seq)
        | _ -> failwith "Failed to parse list"

    [<Fact>]
    let ``parseList - Sep - digits`` () =        
        let p = parseList pDigits (Sep (pComma, false, 0))
        match tryRun p "1,2,3,4,5,6,7" () with
        | RunSuccess(_, result, _) ->
            Assert.Equal(['1'..'7'], result:>char seq)
        | _ -> failwith "Failed to parse list"

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
        match tryRun p text () with
        | RunSuccess (_, value, _) when not shouldThrow ->
            Assert.Equal(length, value.Length)
        | RunFailure (_, value, _) when shouldThrow -> ()
        | _ -> failwith "Failed to parse list."

    [<Fact>]
    let ``parseList - Until`` () =        
        let p = parseList pOne (Until pComma)
        let result = tryRun p "1111," ()
        let expect = ['1';'1';'1';'1']
        match result with
        | RunSuccess (_, value, _) ->
            Assert.Equal (expect,  value:>char seq)
        | _ -> failwith "Failed to parse list."

    [<Fact>]
    let ``parseList - Until - Throws`` () =        
        let p = parseList pOne (Until pComma)
        match tryRun p "111111" () with
        | RunFailure (_, value, _) -> ()
        | _ -> failwith "Failed to parse list."

    [<Theory>]
    [<InlineData("1,1,1,1", 4, true, 0, false)>]
    [<InlineData("1,1,1,1", 4, false, 0, false)>]
    [<InlineData("1,1,1,1,", 4, false, 0, true)>]
    [<InlineData("1,1,1,1", 4, true, 1, false)>]
    [<InlineData("1,1,1,1", 4, false, 1, false)>]
    [<InlineData("1,1,1,1,", 0, false, 1, true)>]
    [<InlineData("", 0, false, 1, true)>]
    [<InlineData("1,1,1,1", 4, true, 2, false)>]
    [<InlineData("1,1,1,1", 4, false, 2, false)>]
    [<InlineData("1,1,1,1,", 4, false, 2, true)>]
    [<InlineData("1", 0, false, 2, true)>]
    let ``parseList - Sep`` text length allowTrailingSep minElements shouldThrow =        
        let p = parseList pOne (Sep(pComma, allowTrailingSep, minElements))
        match tryRun p text () with
        | RunSuccess (_, value, _) when not shouldThrow ->
            Assert.Equal(length, value.Length)
        | RunFailure (_, _, _) when shouldThrow -> ()
        | _ -> failwith "Failed to parse list."