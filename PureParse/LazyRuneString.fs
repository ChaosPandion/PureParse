namespace rec PureParse
open System.Numerics
open System.Runtime.CompilerServices
open FSharp.NativeInterop
open System
open System.IO
open System.Text

module LazyRuneString =
    begin

        type LazyRuneString(text:string, index:int) =
            class

                static member Create(text:string) = LazyRuneString(text, 0)

                member _.Next() =
                    begin
                    end

            end


    end


