namespace PureParse.Test

module EventTreeTests = begin

        open System
        open System.Text
        open PureParse
        open Xunit
        
        let private parserB:Parser<unit, unit> =
            parseProduction "B" 
                <| parse {
                    do! parseProduction "B1" (skipChar 'B')
                    do! parseProduction "B2" (skipChar 'B')
                    do! parseProduction "B3" (skipChar 'B')
                    return ()
                }

        let private parserA:Parser<unit, unit> =
            parseProduction "A" 
                <| parse {
                    do! parseProduction "A1" (skipChar 'A')
                    do! parseProduction "A2" (skipChar 'A')
                    do! parseProduction "A3" (skipChar 'A')
                    do! parserB
                    return ()
                }

        [<Fact>]
        let ``The event tree is correct`` () =
            let result, tree = run2 parserA "AAABBB" ()
            match tree with
            | SucceededProduction({
                    enterData = {parserName="A";message="";index=0;line=1;column=1;timestamp=_;state=_;error=_;}
                    exitData = {parserName="A";message="";index=6;line=1;column=7;timestamp=_;state=_;error=_;}
                    token = _
                    children = [
                        SucceededProduction({
                            enterData = {parserName="A1";message="";index=0;line=1;column=1;timestamp=_;state=_;error=_;}
                            exitData = {parserName="A1";message="";index=1;line=1;column=2;timestamp=_;state=_;error=_;}
                            token = Some("A")
                            children = [ ] })
                        SucceededProduction({
                            enterData = {parserName="A2";message="";index=1;line=1;column=2;timestamp=_;state=_;error=_;}
                            exitData = {parserName="A2";message="";index=2;line=1;column=3;timestamp=_;state=_;error=_;}
                            token = Some("A")
                            children = [ ] })
                        SucceededProduction({
                            enterData = {parserName="A3";message="";index=2;line=1;column=3;timestamp=_;state=_;error=_;}
                            exitData = {parserName="A3";message="";index=3;line=1;column=4;timestamp=_;state=_;error=_;}
                            token = Some("A")
                            children = [ ] })
                        SucceededProduction({
                            enterData = {parserName="B";message="";index=3;line=1;column=4;timestamp=_;state=_;error=_;}
                            exitData = {parserName="B";message="";index=6;line=1;column=7;timestamp=_;state=_;error=_;}
                            token = _
                            children = [ 
                                SucceededProduction({
                                    enterData = {parserName="B1";message="";index=3;line=1;column=4;timestamp=_;state=_;error=_;}
                                    exitData = {parserName="B1";message="";index=4;line=1;column=5;timestamp=_;state=_;error=_;}
                                    token = Some("B")
                                    children = [ ] })
                                SucceededProduction({
                                    enterData = {parserName="B2";message="";index=4;line=1;column=5;timestamp=_;state=_;error=_;}
                                    exitData = {parserName="B2";message="";index=5;line=1;column=6;timestamp=_;state=_;error=_;}
                                    token = Some("B")
                                    children = [ ] })
                                SucceededProduction({
                                    enterData = {parserName="B3";message="";index=5;line=1;column=6;timestamp=_;state=_;error=_;}
                                    exitData = {parserName="B3";message="";index=6;line=1;column=7;timestamp=_;state=_;error=_;}
                                    token = Some("B")
                                    children = [ ] })
                            ] })
                    ] }) -> ()
            | _ -> Assert.True false

        [<Fact>]
        let ``The error is clear within the event tree`` () =
            let result, tree = run2 parserA "AAABB" ()
            match tree with
            | FailedProduction({
                    enterData = {parserName="A";message="";index=0;line=1;column=1;timestamp=_;state=_;error=_;}
                    exitData = {parserName="A";message="";index=3;line=1;column=4;timestamp=_;state=_;error=_;}                        
                    token = _
                    children = [
                        SucceededProduction({
                            enterData = {parserName="A1";message="";index=0;line=1;column=1;timestamp=_;state=_;error=_;}
                            exitData = {parserName="A1";message="";index=1;line=1;column=2;timestamp=_;state=_;error=_;}
                            token = Some("A")
                            children = [ ] })
                        SucceededProduction({
                            enterData = {parserName="A2";message="";index=1;line=1;column=2;timestamp=_;state=_;error=_;}
                            exitData = {parserName="A2";message="";index=2;line=1;column=3;timestamp=_;state=_;error=_;}
                            token = Some("A")
                            children = [ ] })
                        SucceededProduction({
                            enterData = {parserName="A3";message="";index=2;line=1;column=3;timestamp=_;state=_;error=_;}
                            exitData = {parserName="A3";message="";index=3;line=1;column=4;timestamp=_;state=_;error=_;}
                            token = Some("A")
                            children = [ ] })
                        FailedProduction({
                            enterData = {parserName="B";message="";index=3;line=1;column=4;timestamp=_;state=_;error=_;}
                            exitData = {parserName="B";message="";index=5;line=1;column=6;timestamp=_;state=_;error=_;}
                            children = [ 
                                SucceededProduction({
                                    enterData = {parserName="B1";message="";index=3;line=1;column=4;timestamp=_;state=_;error=_;}
                                    exitData = {parserName="B1";message="";index=4;line=1;column=5;timestamp=_;state=_;error=_;}
                                    token = Some("B")
                                    children = [ ] })
                                SucceededProduction({
                                    enterData = {parserName="B2";message="";index=4;line=1;column=5;timestamp=_;state=_;error=_;}
                                    exitData = {parserName="B2";message="";index=5;line=1;column=6;timestamp=_;state=_;error=_;}
                                    token = Some("B")
                                    children = [ ] })
                                FailedProduction({
                                    enterData = {parserName="B3";message="";index=5;line=1;column=6;timestamp=_;state=_;error=_;}
                                    exitData = {parserName="B3";message="";index=5;line=1;column=6;timestamp=_;state=_;error=_;}
                                    token = _
                                    children = [ ] })
                            ] })
                    ] }) -> ()
            | _ -> Assert.True false
            

        [<Fact>]
        let ``Early error means a smaller tree.`` () =
            let result, tree = run2 parserA "AA" ()
            match tree with
            | FailedProduction({
                    enterData = {parserName="A";message="";index=0;line=1;column=1;timestamp=_;state=_;error=_;}
                    exitData = {parserName="A";message="";index=2;line=1;column=3;timestamp=_;state=_;error=_;}
                    token = _
                    children = [
                        SucceededProduction({
                            enterData = {parserName="A1";message="";index=0;line=1;column=1;timestamp=_;state=_;error=_;}
                            exitData = {parserName="A1";message="";index=1;line=1;column=2;timestamp=_;state=_;error=_;}
                            token = Some("A")
                            children = [ ] })
                        SucceededProduction({
                            enterData = {parserName="A2";message="";index=1;line=1;column=2;timestamp=_;state=_;error=_;}
                            exitData = {parserName="A2";message="";index=2;line=1;column=3;timestamp=_;state=_;error=_;}
                            token = Some("A")
                            children = [ ] })
                        FailedProduction({
                            enterData = {parserName="A3";message="";index=2;line=1;column=3;timestamp=_;state=_;error=_;}
                            exitData = {parserName="A3";message="";index=2;line=1;column=3;timestamp=_;state=_;error=_;}
                            token = _
                            children = [ ] })
                    ] }) -> ()
            | _ -> Assert.True false
            

    end

