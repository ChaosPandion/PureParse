﻿

open PureParse;


//let s1 = "____________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________"
//let s2 = "____________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________asdasdasdasdasdasdasdasdasdasdasdasdasasdddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd"

//let p1 = parseString s1
//let p2 = parseString3 s1

//let state = { text = s2; index = 0 }



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

let controlChars = controlChars
let asciiChars = asciiChars
let unicodeWhiteSpaceChars = unicodeWhiteSpaceChars
let unicodeLetterChars = unicodeLetterChars
let unicodeDigitChars = unicodeDigitChars

let asciiDigitNoZeroChars = asciiDigitNoZeroChars
let asciiDigitChars = asciiDigitChars
let asciiLowerLetterChars = asciiLowerLetterChars
let asciiUpperLetterChars = asciiUpperLetterChars
let asciiLetterChars = asciiLetterChars

for c in PureParse.CharSets.unicodeDigitChars do
    printfn "%c" c

System.Console.ReadLine() |> ignore