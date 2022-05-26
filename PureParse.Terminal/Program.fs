

open PureParse;
open PureParse.Examples

open System
open System.IO

let json = File.ReadAllText("c:/test.json/5c3935c8ab777f04f83f272425b750f9.json")
let x1 = ResizeArray(10000000)
let x2 = ResizeArray(10000000)
for i in [0..20] do
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let r1 = Json.parseText json
    sw.Stop()
    x2.Add(sw.Elapsed)
    x1.Add(r1)
    printfn "%O" (sw.Elapsed)

let s = x2 |> Seq.averageBy (fun e -> e.TotalMilliseconds)
printfn "%O" (TimeSpan.FromMilliseconds(s))
//let x1 = ResizeArray(10000000)
//let sw = System.Diagnostics.Stopwatch.StartNew()
//for i in [0..10000000] do
//    let r1 = p1 state
//    x1.Add(r1)
//sw.Stop();
//printfn "%s" (sw.Elapsed.ToString())

//let x2 = ResizeArray(10000000)
//let sw1 = System.Diagnostics.Stopwatch.StartNew()
//for i in [0..10000000] do
//    let r2 = p2 state
//    x2.Add(r2)
//sw1.Stop();
//printfn "%s" (sw1.Elapsed.ToString())

//let controlChars = controlChars
//let asciiChars = asciiChars
//let unicodeWhiteSpaceChars = unicodeWhiteSpaceChars
//let unicodeLetterChars = unicodeLetterChars
//let unicodeDigitChars = unicodeDigitChars

//let asciiDigitNoZeroChars = asciiDigitNoZeroChars
//let asciiDigitChars = asciiDigitChars
//let asciiLowerLetterChars = asciiLowerLetterChars
//let asciiUpperLetterChars = asciiUpperLetterChars
//let asciiLetterChars = asciiLetterChars

//for c in PureParse.CharSets.unicodeDigitChars do
//    printfn "%c" c

System.Console.ReadLine() |> ignore