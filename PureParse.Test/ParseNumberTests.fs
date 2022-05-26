namespace PureParse.Test

module ParseNumberTests =

    open System
    open Xunit
    open PureParse.Examples.Number

    [<Theory>]
    [<InlineData(0L, "0")>]
    [<InlineData(1L, "1")>]
    [<InlineData(10L, "10")>]
    [<InlineData(100L, "100")>]
    [<InlineData(+0L, "+0")>]
    [<InlineData(+1L, "+1")>]
    [<InlineData(+10L, "+10")>]
    [<InlineData(+100L, "+100")>]
    [<InlineData(-0L, "-0")>]
    [<InlineData(-1L, "-1")>]
    [<InlineData(-10L, "-10")>]
    [<InlineData(-100L, "-100")>]
    let ``Parse Decimal Integer`` expect text =
        let result = parseInt64 text
        Assert.Equal (expect, result)

    [<Theory>]
    [<InlineData(0x0L, "0x0")>]
    [<InlineData(0x1L, "0x1")>]
    [<InlineData(0x2L, "0x2")>]
    [<InlineData(0x3L, "0x3")>]
    [<InlineData(0x4L, "0x4")>]
    [<InlineData(0x5L, "0x5")>]
    [<InlineData(0x6L, "0x6")>]
    [<InlineData(0x7L, "0x7")>]
    [<InlineData(0x8L, "0x8")>]
    [<InlineData(0x9L, "0x9")>]
    [<InlineData(0xAL, "0xA")>]
    [<InlineData(0xBL, "0xB")>]
    [<InlineData(0xCL, "0xC")>]
    [<InlineData(0xDL, "0xD")>]
    [<InlineData(0xEL, "0xE")>]
    [<InlineData(0xFL, "0xF")>]
    [<InlineData(0x0AL, "0x0A")>]
    [<InlineData(0x1AL, "0x1A")>]
    [<InlineData(0x2AL, "0x2A")>]
    [<InlineData(0x3AL, "0x3A")>]
    [<InlineData(0x4AL, "0x4A")>]
    [<InlineData(0x5AL, "0x5A")>]
    [<InlineData(0x6AL, "0x6A")>]
    [<InlineData(0x7AL, "0x7A")>]
    [<InlineData(0x8AL, "0x8A")>]
    [<InlineData(0x9AL, "0x9A")>]
    [<InlineData(0xAAL, "0xAA")>]
    [<InlineData(0xBAL, "0xBA")>]
    [<InlineData(0xCAL, "0xCA")>]
    [<InlineData(0xDAL, "0xDA")>]
    [<InlineData(0xEAL, "0xEA")>]
    [<InlineData(0xFAL, "0xFA")>]
    [<InlineData(-0xFAL, "-0xFA")>]
    [<InlineData(+0xFAL, "+0xFA")>]
    let ``Parse Hex Integer`` expect text =
        let result = parseInt64 text
        Assert.Equal (expect, result)

    [<Theory>]
    [<InlineData(0b0, "0b0")>]
    [<InlineData(0b1, "0b1")>]
    [<InlineData(0b01, "0b01")>]
    [<InlineData(0b101, "0b101")>]
    [<InlineData(0b0101, "0b0101")>]
    [<InlineData(-0b0101, "-0b0101")>]
    [<InlineData(+0b0101, "+0b0101")>]
    let ``Parse Binary Integer`` expect text =
        let result = parseInt64 text
        Assert.Equal (expect, result)

    [<Theory>]
    [<InlineData(0.0, "0")>]
    [<InlineData(1.0, "1")>]
    [<InlineData(10.0, "10")>]
    [<InlineData(100.0, "100")>]
    [<InlineData(100000000.0, "100000000")>]
    [<InlineData(1.1, "1.1")>]
    [<InlineData(1.12, "1.12")>]
    [<InlineData(1.123, "1.123")>]
    [<InlineData(-1.1, "-1.1")>]
    [<InlineData(-1.12, "-1.12")>]
    [<InlineData(-1.123, "-1.123")>]
    [<InlineData(1.1e1, "1.1e1")>]
    [<InlineData(1.12e1, "1.12e1")>]
    [<InlineData(1.123e1, "1.123e1")>]
    [<InlineData(1.1e+1, "1.1e+1")>]
    [<InlineData(1.12e+1, "1.12e+1")>]
    [<InlineData(1.123e+1, "1.123e+1")>]
    [<InlineData(1.1e-1, "1.1e-1")>]
    [<InlineData(1.12e-1, "1.12e-1")>]
    [<InlineData(1.123e-1, "1.123e-1")>]
    [<InlineData(1023.999e+99, "1023.999e+99")>]
    [<InlineData(-1023.999e+99, "-1023.999e+99")>]
    [<InlineData(1.2e-99, "1.2e-99")>]
    [<InlineData(1.2E-99, "1.2E-99")>]
    let ``Parse Float`` expect text =
        let result = parseDouble text
        match result with
        | n when Math.Abs(expect - n) = 0 ->
            Assert.Equal(expect, n)
        | n when Math.Abs(expect - n) > 0.00000001 ->
            Assert.Equal(expect, n)
        | _ -> () // Success
