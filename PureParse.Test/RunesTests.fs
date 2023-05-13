namespace PureParse.Test

open System
open System.Text
open PureParse
open Xunit

module RunesTests =

    [<Theory>]
    [<InlineData('0', true)>]
    [<InlineData('1', true)>]
    [<InlineData('2', true)>]
    [<InlineData('3', true)>]
    [<InlineData('4', true)>]
    [<InlineData('5', true)>]
    [<InlineData('6', true)>]
    [<InlineData('7', true)>]
    [<InlineData('8', true)>]
    [<InlineData('9', true)>]
    [<InlineData('A', false)>]
    [<InlineData('B', false)>]
    [<InlineData('!', false)>]
    [<InlineData(' ', false)>]
    let ``isDigit tests`` (c:char) succeed =
        Assert.Equal(succeed, isDigit (Rune c))

    [<Theory>]
    [<InlineData('1', true)>]
    [<InlineData('2', true)>]
    [<InlineData('3', true)>]
    [<InlineData('4', true)>]
    [<InlineData('5', true)>]
    [<InlineData('6', true)>]
    [<InlineData('7', true)>]
    [<InlineData('8', true)>]
    [<InlineData('9', true)>]
    [<InlineData('0', false)>]
    [<InlineData('A', false)>]
    [<InlineData('B', false)>]
    [<InlineData('!', false)>]
    [<InlineData(' ', false)>]
    let ``isNonZeroDigit tests`` (c:char) succeed =
        Assert.Equal(succeed, isNonZeroDigit (Rune c))

    [<Theory>]
    [<InlineData('0', true)>]
    [<InlineData('1', true)>]
    [<InlineData('2', true)>]
    [<InlineData('3', true)>]
    [<InlineData('4', true)>]
    [<InlineData('5', true)>]
    [<InlineData('6', true)>]
    [<InlineData('7', true)>]
    [<InlineData('8', true)>]
    [<InlineData('9', true)>]
    [<InlineData('a', true)>]
    [<InlineData('b', true)>]
    [<InlineData('c', true)>]
    [<InlineData('d', true)>]
    [<InlineData('e', true)>]
    [<InlineData('f', true)>]
    [<InlineData('A', true)>]
    [<InlineData('B', true)>]
    [<InlineData('C', true)>]
    [<InlineData('D', true)>]
    [<InlineData('E', true)>]
    [<InlineData('F', true)>]
    [<InlineData('X', false)>]
    [<InlineData('Y', false)>]
    [<InlineData('Z', false)>]
    [<InlineData('!', false)>]
    [<InlineData(' ', false)>]
    let ``isHexDigit tests`` (c:char) succeed =
        Assert.Equal(succeed, isHexDigit (Rune c))

    [<Fact>]
    let ``Runes pattern is correct`` () =
        let r = "AAA".EnumerateRunes() |> Seq.toArray |> ReadOnlyMemory 
        match r with
        | Runes "AAA" -> ()
        | _ -> failwith "Failed to match"
        let expect = "AAA".EnumerateRunes() |> Seq.toArray |> ReadOnlyMemory
        match r with
        | Runes expect -> ()
        | _ -> failwith "Failed to match"

