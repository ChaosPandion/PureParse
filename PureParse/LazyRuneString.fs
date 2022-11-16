namespace rec PureParse
open System.Numerics
open System.Runtime.CompilerServices
open FSharp.NativeInterop
open System
open System.IO
open System.Text

module LazyRuneString =
    begin

        type LazyRuneStream(value:StreamReader) =
            class

                [<Literal>] 
                let spanLength:int = 1024

                let span1:Span<byte> = Span<byte>(Array.zeroCreate<byte> spanLength)
                let length1:int = 0
                let span2:Span<byte> = Span<byte>(Array.zeroCreate<byte> spanLength)
                let length2:int = 0

                member _.Next() =
                    begin


                    end

            end


    end


