namespace PureParse
#nowarn "9"


[<AutoOpen>]
module Runes =

    open System
    open System.Linq
    open System.Text
    
    type RuneData =
    | RuneSet of Set<Rune>
    | RuneSeq of seq<Rune>
    | RuneCharSeq of seq<char>
    | RuneString of string
    | RuneCharArray of char[]

    /// Map the rune to an char
    let toChar (value:Rune) =
        if value.Utf16SequenceLength <> 1 then
            failwith "Not a valid UTF-16 char"
        else
            char value.Value 

    let inline isDigit r = 
        r >= Rune('0') && r <= Rune('9')

    let inline isNonZeroDigit r = 
        r >= Rune('1') && r <= Rune('9')

    let inline isHexDigit r = 
        (r >= Rune('0') && r <= Rune('9')) || 
        (r >= Rune('a') && r <= Rune('f')) || 
        (r >= Rune('A') && r <= Rune('F'))    

    /// Map the rune to an char
    let (|RuneChar|) (value:Rune) =
        if value.Utf16SequenceLength <> 1 then
            failwith "Not a valid UTF-16 char"
        else
            char value.Value 

    /// Map the rune to an int32
    let inline (|RuneValue|) (value:Rune) =
        value.Value 

    /// Map the readonly memory of runes to a string.
    let inline (|RuneString|) (value:ReadOnlyMemory<Rune>) =           
        let sb = StringBuilder(value.Length)
        for r in value.Span do
            sb.Append(r.ToString()) |> ignore
        sb.ToString ()            
    
    /// Match the input Rune against the expectation
    [<return: Struct>]
    let (|Runes|_|) (expect:obj) (value:ReadOnlyMemory<Rune>) =
        match expect with
        | :? String as s ->
            if s = null then
                nullArg (nameof expect)
            if Enumerable.SequenceEqual(s.EnumerateRunes(), value.ToArray())
            then ValueSome Runes
            else ValueNone
        | :? ReadOnlyMemory<Rune> as rm ->
            if rm.Length = value.Length && value.Span.SequenceEqual(rm.Span) 
            then ValueSome Runes
            else ValueNone
        | _ -> ValueNone

    /// Match the input Rune against the expectation
    [<return: Struct>]
    let inline (|Rune|_|) (expect:obj) (value:Rune) =
        match expect with
        | :? String as s ->
            if s = null then
                nullArg (nameof expect)
            if Rune.GetRuneAt(s, 0) = value
            then ValueSome Rune
            else ValueNone
        | :? Char as c ->
            let success, rune = Rune.TryCreate(c)
            if not success then
                invalidArg (nameof expect) "Invalid Rune"
            if rune = value
            then ValueSome Rune
            else ValueNone
        | _ -> 
            failwith "Type of expect is not supported."

        