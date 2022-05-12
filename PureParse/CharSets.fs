namespace PureParse

[<AutoOpen>]
module CharSets =

    open System

    let unicodeWhiteSpaceChars = [Char.MinValue..Char.MaxValue] |> Seq.filter (Char.IsWhiteSpace) |> Set.ofSeq
    let unicodeLetterChars = [Char.MinValue..Char.MaxValue] |> Seq.filter (Char.IsLetter) |> Set.ofSeq
    let unicodeDigitChars = [Char.MinValue..Char.MaxValue] |> Seq.filter (Char.IsDigit) |> Set.ofSeq
    let controlChars = [Char.MinValue..Char.MaxValue] |> Seq.filter (Char.IsControl) |> Set.ofSeq
    let asciiChars = [Char.MinValue..Char.MaxValue] |> Seq.filter (Char.IsAscii) |> Set.ofSeq
    let asciiWhiteSpace = asciiChars |> Seq.except unicodeWhiteSpaceChars |> Set.ofSeq
    let asciiDigitNoZeroChars = [ '1'..'9' ] |> Set.ofList
    let asciiDigitChars = [ '0'..'9' ] |> Set.ofList
    let asciiLowerLetterChars = [ 'a'..'z' ] |> Set.ofList
    let asciiUpperLetterChars = [ 'A'..'Z' ] |> Set.ofList
    let asciiLetterChars = ([ 'a'..'z' ] @ [ 'A'..'Z' ]) |> Set.ofSeq
