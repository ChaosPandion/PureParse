namespace PureParse.Test

open System
open System.Text
open PureParse
open Xunit

module NumberParserTests =    
                        
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

        [<Theory>]
        [<InlineData("0", 0.0)>]
        [<InlineData("1", 1.0)>]
        [<InlineData("2", 2.0)>]
        [<InlineData("3", 3.0)>]
        [<InlineData("4", 4.0)>]
        [<InlineData("5", 5.0)>]
        [<InlineData("10", 10.0)>]
        [<InlineData("11", 11.0)>]
        [<InlineData("12", 12.0)>]
        [<InlineData("13", 13.0)>]
        [<InlineData("14", 14.0)>]
        [<InlineData("15", 15.0)>]
        [<InlineData("100", 100.0)>]
        [<InlineData("110", 110.0)>]
        [<InlineData("120", 120.0)>]
        [<InlineData("130", 130.0)>]
        [<InlineData("140", 140.0)>]
        [<InlineData("150", 150.0)>]
        [<InlineData("0.0", 0.0)>]
        [<InlineData("1.0", 1.0)>]
        [<InlineData("2.0", 2.0)>]
        [<InlineData("3.0", 3.0)>]
        [<InlineData("4.0", 4.0)>]
        [<InlineData("5.0", 5.0)>]
        [<InlineData("1.1", 1.1)>]
        [<InlineData("100.1999", 100.1999)>]
        [<InlineData("-2", -2.0)>]
        [<InlineData("-1.1", -1.1)>]
        [<InlineData("-100.1999", -100.1999)>]
        [<InlineData("+2", +2.0)>]
        [<InlineData("+1.1", +1.1)>]
        [<InlineData("+100.1999", +100.1999)>]
        [<InlineData("2e2", 2e2)>]
        [<InlineData("2e+2", 2e+2)>]
        [<InlineData("2e-2", 2e-2)>]
        [<InlineData("2.123e2", 2.123e2)>]
        [<InlineData("2.123e+2", 2.123e+2)>]
        [<InlineData("2.123e-2", 2.123e-2)>]
        let ``parseFloat returns the expected result.`` text expect =  
            let p = parse {
                let! r = parseFloat ()
                return r
            }
            match tryRun p text () with
            | RunSuccess (_, e, _) ->
                Assert.Equal(expect, e)
            | _ -> failwithf "Unknown Result"

        [<Theory>]
        [<InlineData("0", 0.0)>]
        [<InlineData("1", 1.0)>]
        [<InlineData("2", 2.0)>]
        [<InlineData("3", 3.0)>]
        [<InlineData("4", 4.0)>]
        [<InlineData("5", 5.0)>]
        [<InlineData("10", 10.0)>]
        [<InlineData("11", 11.0)>]
        [<InlineData("12", 12.0)>]
        [<InlineData("13", 13.0)>]
        [<InlineData("14", 14.0)>]
        [<InlineData("15", 15.0)>]
        [<InlineData("100", 100.0)>]
        [<InlineData("110", 110.0)>]
        [<InlineData("120", 120.0)>]
        [<InlineData("130", 130.0)>]
        [<InlineData("140", 140.0)>]
        [<InlineData("150", 150.0)>]
        [<InlineData("0.0", 0.0)>]
        [<InlineData("1.0", 1.0)>]
        [<InlineData("2.0", 2.0)>]
        [<InlineData("3.0", 3.0)>]
        [<InlineData("4.0", 4.0)>]
        [<InlineData("5.0", 5.0)>]
        [<InlineData("1.1", 1.1)>]
        [<InlineData("100.1999", 100.1999)>]
        [<InlineData("-2", -2.0)>]
        [<InlineData("-1.1", -1.1)>]
        [<InlineData("-100.1999", -100.1999)>]
        [<InlineData("+2", +2.0)>]
        [<InlineData("+1.1", +1.1)>]
        [<InlineData("+100.1999", +100.1999)>]
        [<InlineData("2e2", 2e2)>]
        [<InlineData("2e+2", 2e+2)>]
        [<InlineData("2e-2", 2e-2)>]
        [<InlineData("2.123e2", 2.123e2)>]
        [<InlineData("2.123e+2", 2.123e+2)>]
        [<InlineData("2.123e-2", 2.123e-2)>]
        let ``parseDecimal returns the expected result.`` text expect =  
            match tryRun (parseDecimal ()) text () with
            | RunSuccess (_, e, _) ->
                Assert.Equal(expect, e)
            | _ -> failwithf "Unknown Result"

        [<Theory>]
        [<InlineData("0", 0.0)>]
        [<InlineData("1", 1.0)>]
        [<InlineData("2", 2.0)>]
        [<InlineData("3", 3.0)>]
        [<InlineData("4", 4.0)>]
        [<InlineData("5", 5.0)>]
        [<InlineData("10", 10.0)>]
        [<InlineData("11", 11.0)>]
        [<InlineData("12", 12.0)>]
        [<InlineData("13", 13.0)>]
        [<InlineData("14", 14.0)>]
        [<InlineData("15", 15.0)>]
        [<InlineData("100", 100.0)>]
        [<InlineData("110", 110.0)>]
        [<InlineData("120", 120.0)>]
        [<InlineData("130", 130.0)>]
        [<InlineData("140", 140.0)>]
        [<InlineData("150", 150.0)>]
        [<InlineData("0.0", 0.0)>]
        [<InlineData("1.0", 1.0)>]
        [<InlineData("2.0", 2.0)>]
        [<InlineData("3.0", 3.0)>]
        [<InlineData("4.0", 4.0)>]
        [<InlineData("5.0", 5.0)>]
        [<InlineData("1.1", 1.1)>]
        [<InlineData("100.1999", 100.1999)>]
        [<InlineData("-2", -2.0)>]
        [<InlineData("-1.1", -1.1)>]
        [<InlineData("-100.1999", -100.1999)>]
        [<InlineData("+2", +2.0)>]
        [<InlineData("+1.1", +1.1)>]
        [<InlineData("+100.1999", +100.1999)>]
        [<InlineData("2e2", 2e2)>]
        [<InlineData("2e+2", 2e+2)>]
        [<InlineData("2e-2", 2e-2)>]
        [<InlineData("2.123e2", 2.123e2)>]
        [<InlineData("2.123e+2", 2.123e+2)>]
        [<InlineData("2.123e-2", 2.123e-2)>]
        let ``parseHalf returns the expected result.`` text expect =   
            let p = parse {
                let! r = parseReal<unit, Half> ()
                return r
            }
            match tryRun p text () with
            | RunSuccess (_, e, _) ->
                Assert.Equal(expect, e)
            | _ -> failwithf "Unknown Result"

        [<Theory>]
        [<InlineData(-1)>]
        [<InlineData(-2)>]
        [<InlineData(-3)>]
        [<InlineData(-4)>]
        [<InlineData(-5)>]
        [<InlineData(-6)>]
        [<InlineData(-7)>]
        [<InlineData(-8)>]
        [<InlineData(-9)>]
        [<InlineData(-10)>]
        let ``Base10PowerFunction does not support negative powers`` n =
            Assert.Throws<ArgumentOutOfRangeException>(fun () -> Base10PowerFunction<int64>.Calculate n |> ignore) |> ignore
            
        [<Theory>]
        [<InlineData(0)>]
        [<InlineData(1)>]
        [<InlineData(2)>]
        [<InlineData(3)>]
        [<InlineData(4)>]
        [<InlineData(5)>]
        [<InlineData(6)>]
        [<InlineData(7)>]
        [<InlineData(8)>]
        [<InlineData(9)>]
        [<InlineData(10)>]
        let ``Base10PowerFunction is correct`` n =
            Assert.Equal(pown 10L n, Base10PowerFunction<int64>.Calculate n)
            //Assert.Equal(10L, Base10PowerFunction<int64>.Calculate 1)
            //Assert.Equal(100L, Base10PowerFunction<int64>.Calculate 2)
            //Assert.Equal(1000L, Base10PowerFunction<int64>.Calculate 3)
            //Assert.Equal(10000L, Base10PowerFunction<int64>.Calculate 4)
            //Assert.Equal(100000L, Base10PowerFunction<int64>.Calculate 5)
            //Assert.Equal(1000000L, Base10PowerFunction<int64>.Calculate 6)
            //Assert.Equal(10000000L, Base10PowerFunction<int64>.Calculate 7)
            //Assert.Equal(100000000L, Base10PowerFunction<int64>.Calculate 8)
            //Assert.Equal(1000000000L, Base10PowerFunction<int64>.Calculate 9)
            //Assert.Equal(10000000000L, Base10PowerFunction<int64>.Calculate 10)
            //Assert.Equal(100000000000L, Base10PowerFunction<int64>.Calculate 11)
            //Assert.Equal(1000000000000L, Base10PowerFunction<int64>.Calculate 12)

