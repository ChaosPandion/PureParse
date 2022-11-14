namespace PureParse.Test

module ListParserTests =

    open System
    open Xunit
    open PureParse
    
    let pDigits:Parser<unit, char> = map (parseAnyOf (RuneCharSeq "0123456789")) (fun r -> char r.Value)
    let pOne:Parser<unit, char> = parseChar '1'
    let pComma:Parser<unit, unit> = skipChar ','
    let pSemiColon:Parser<unit, unit> = skipChar ';'

    [<Fact>]
    let ``parseList - Many - digits`` () =        
        let p = parseList pDigits (Many 0)
        match tryRun p "1234567" () with
        | RunSuccess(_, result, _) ->
            Assert.Equal(['1'..'7'], result:>char seq)
        | _ -> failwith "Failed to parse list"

    [<Fact>]
    let ``parseList - Until - digits`` () =        
        let p = parseList pDigits (Until (pComma, 0))
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

    [<Fact>]
    let ``parseList - SepUntil - digits`` () =        
        let p = parseList pDigits (SepUntil (pComma, pSemiColon, false, 0))
        match tryRun p "1,2,3;" () with
        | RunSuccess(_, ['1';'2';'3'], _) -> ()
        | _ -> failwith "Failed to parse list"
        let p = parseList pDigits (SepUntil (pComma, pSemiColon, true, 0))
        match tryRun p "1,2,3,;" () with
        | RunSuccess(_, ['1';'2';'3'], _) -> ()
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

    [<Theory>]
    [<InlineData("", false)>]
    [<InlineData("X;", true)>]
    [<InlineData("XX;", true)>]
    [<InlineData("XXX;", true)>]
    [<InlineData("XXXX;", true)>]
    [<InlineData("XXXXX;", true)>]
    [<InlineData("XXXXXX;", true)>]
    [<InlineData("XXXXXX", false)>]
    [<InlineData("XXXXX", false)>]
    [<InlineData("XXXX", false)>]
    [<InlineData("XXX", false)>]
    [<InlineData("XX", false)>]
    [<InlineData("X", false)>]
    let ``parseList - Until`` text succeed =    
        let p = parseString "X"
        let ending = parseChar ';' >>= fun _ -> result ()
        match tryRun (parseList p (Until(ending, 0))) text () with
        | RunFailure (_, _, _) when not succeed -> ()
        | RunSuccess (_, _, _) when succeed -> ()
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

    [<Theory>]
    [<InlineData("", false, 0, false)>]
    [<InlineData(";", false, 0, true)>]
    [<InlineData("X;", false, 0, true)>]
    [<InlineData("X,X,X;", false, 1, true)>]
    [<InlineData("X,X,X;", true, 1, true)>]
    [<InlineData("X,X,X;", false, 2, true)>]
    [<InlineData("X,X,X;", true, 2, true)>]
    [<InlineData("X,X,X;", false, 3, true)>]
    [<InlineData("X,X,X,X;", false, 10, false)>]
    [<InlineData("X,X,X,X,X,X,X,X,X", false, 10, false)>]
    [<InlineData("X,X,X,X,X,X,X,X,X,", false, 10, false)>]
    [<InlineData("X,X,X,X,X,X,X,X,X,A", false, 10, false)>]
    [<InlineData("X,X,X,X,X,X,X,X,X,;", true, 1, true)>]
    let ``parseList - SepUntil`` text allowTrailingSeparator minElements succeed =    
        let p = parseString "X"
        let sep = parseChar ',' >>= fun _ -> result ()
        let ending = parseChar ';' >>= fun _ -> result ()
        match tryRun (parseList p (SepUntil(sep, ending, allowTrailingSeparator, minElements))) text () with
        | RunFailure (_, _, _) when not succeed -> ()
        | RunSuccess (_, _, _) when succeed -> ()
        | _ -> failwith "Failed to parse list."

    [<Theory>]
    [<InlineData("", 0, true)>]
    [<InlineData("", 1, false)>]
    [<InlineData("X", 1, true)>]
    [<InlineData("XX", 2, true)>]
    [<InlineData("XX", 3, false)>]
    [<InlineData("XXXXXX", 6, true)>]
    [<InlineData("XXXXXXXXXXXX", 12, true)>]
    let ``parseList - ExactCount`` text count succeed =    
        let p = parseString "X"
        match tryRun (parseList p (ExactCount count)) text () with
        | RunFailure (_, _, _) when not succeed -> ()
        | RunSuccess (_, _, _) when succeed -> ()
        | _ -> failwith "Failed to parse list."