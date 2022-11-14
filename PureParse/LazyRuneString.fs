namespace rec PureParse
open System.Numerics
open System.Runtime.CompilerServices
open FSharp.NativeInterop
open System
open System.IO
open System.Text

module LazyRuneString =
    begin

        [<Struct>] type StringState = { pointer:voidptr; length:int; index:int }


        type LazyRuneString(value:StreamReader) =
            class
                let s = ReadOnlySpan()

            end


    end


