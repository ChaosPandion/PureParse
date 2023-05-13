namespace PureParse.Test

open System
open System.Text
open PureParse
open Xunit

module ParseTests =
    begin

        let p1:Parser<unit, int> = parseChar '1' ||> string ||> int

        [<Fact>]
        let ``Bind 1 results in a success`` () =
            begin
                let p = parse {
                    let! v1 = p1
                    return v1
                }
                Assert.Equal(1, (run p "1" ()))
            end
        
        [<Fact>]
        let ``Bind 2 computations together results in a success`` () =
            begin
                let p = parse {
                    let! v1 = p1
                    and! v2 = p1
                    let r1 = v1 + v2
                    let! v3 = p1
                    and! v4 = p1
                    let r2 = v3 + v4 
                    return r1 + r2
                }
                Assert.Equal(4, (run p "1111" ()))
            end
        
        [<Fact>]
        let ``Bind 3 computations together results in a success`` () =
            begin
                let p = parse {
                    let! v1 = p1
                    and! v2 = p1
                    and! v3 = p1
                    let r1 = v1 + v2 + v3
                    let! v4 = p1
                    and! v5 = p1
                    and! v6 = p1
                    let r2 = v4 + v5 + v6 
                    return r1 + r2
                }
                Assert.Equal(6, (run p "111111" ()))
            end
        
        [<Fact>]
        let ``Bind 4 computations together results in a success`` () =
            begin
                let p = parse {
                    let! v1 = p1
                    and! v2 = p1
                    and! v3 = p1
                    and! v4 = p1
                    let r1 = v1 + v2 + v3 + v4
                    let! v5 = p1
                    and! v6 = p1
                    and! v7 = p1
                    and! v8 = p1
                    let r2 = v5 + v6 + v7 + v8
                    return r1 + r2
                }
                Assert.Equal(8, (run p "11111111" ()))
            end

        [<Fact>]
        let ``Bind 5 computations together results in a success`` () =
            begin
                let p = parse {
                    let! v1 = p1
                    and! v2 = p1
                    and! v3 = p1
                    and! v4 = p1
                    and! v5 = p1
                    let r1 = v1 + v2 + v3 + v4 + v5
                    let! v6 = p1
                    and! v7 = p1
                    and! v8 = p1
                    and! v9 = p1
                    and! v10 = p1
                    let r2 = v6 + v7 + v8 + v9 + v10
                    return r1 + r2
                }
                Assert.Equal(10, (run p "1111111111" ()))
            end



        [<Fact>]
        let ``run fails when passed null`` () =
            Assert.Throws<ArgumentNullException>(fun () -> run (parseChar 'A') null () |> ignore)

        [<Fact>]
        let ``run throws PureParseException on failure.`` () =
            Assert.Throws<PureParseException<unit>>(fun () -> run (parseChar 'A') "B" () |> ignore)

        [<Fact>]
        let ``Test zero path in parser`` () =
            let p:Parser<unit, string> = parse {
                let! a = parseChar 'A'
                let mutable x = 1
                if x = 1 then
                    x <- x + 1
                let! b = parseChar 'B'
                return $"{x}{a}{b}"
            }
            let s = run p "AB" ()
            Assert.Equal("2AB", s)

        [<Fact>]
        let ``A zero path that is not followed is processed properly`` () =
            let p:Parser<unit, string> = parse {
                let! a = parseChar 'A'
                let mutable x = 1
                if x = 2 then
                    x <- x + 1
                let! b = parseChar 'B'
                return $"{x}{a}{b}"
            }
            let s = run p "AB" ()
            Assert.Equal("1AB", s)
    
        [<Fact>]
        let ``The try pattern allows you to continue`` () =
            let mutable m = ""
            let p:Parser<unit, int> = 
                parse {
                    try 
                        failwith "test"
                        return 1
                    with 
                    | ex -> 
                        m <- ex.ToString()
                        return 2
                }
            match tryRun p "" () with
            | RunSuccess (_, 2, _) ->
                Assert.False(String.IsNullOrEmpty m)
            | _ -> failwith "Expecting Success"

        [<Fact>]
        let ``The try finally pattern allows you to continue`` () =
            let mutable f = false
            let p:Parser<unit, int> = 
                parse {
                    try 
                        return 1
                    finally 
                        f <- true
                }
            match tryRun p "" () with
            | RunSuccess (_, 1, _) when f -> ()
            | _ -> failwith "Expecting Success"

    end

