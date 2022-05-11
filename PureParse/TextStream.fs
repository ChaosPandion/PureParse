namespace PureParse
#nowarn "9"
open System
open System.Text
open System.Buffers
open FSharp.NativeInterop
open System.Numerics;
open System.Runtime.Intrinsics;

module TextStream = 
    begin

        type TextStreamState<'TState> = { 
            /// Any extra state you need to thread through the parser.
            state: 'TState;

            /// The underlying text being processed
            text:string; 
            index:int; 
            line:int 
        }

        type ITextStream<'TState, 'TContent> = interface
            abstract member State : 'TState
            abstract member Index : int
            abstract member Line : int
            abstract member Column : int
            abstract member Remaining : int
            abstract member IsComplete : bool
            abstract member Value : ValueOption<'TContent>
            abstract member Peek : unit -> ValueOption<'TContent>
            abstract member Peek : count:int -> ValueOption<ReadOnlyMemory<'TContent>>
            abstract member Next : unit -> ValueOption<'TContent * ITextStream<'TState, 'TContent>>
            abstract member Next : count:int -> ValueOption<ReadOnlyMemory<'TContent> * ITextStream<'TState, 'TContent>>
        end

        type ByteRuneTextStream<'TState> (state: 'TState, memory:ReadOnlyMemory<byte>, index: int, line: int) = 

            let newline = new Rune('\n')

            static member Create<'TState> (state: 'TState, text:string) =
                if text = null then
                    nullArg (nameof(text))
                let data = new BinaryData(text.ReplaceLineEndings("\n"))
                let memory = data.ToMemory()
                ByteRuneTextStream<'TState>(state, memory, 0, 0)

            member _.State = state

            member _.Index = index

            member _.Line = line

            member inline x.Rune = x.Peek()
                
            member _.Peek () : ValueOption<Rune> =
                match Rune.DecodeFromUtf8(memory.Slice(index).Span) with
                | (Buffers.OperationStatus.Done, rune, _) -> ValueSome rune
                | _ -> ValueNone

            member _.Peek (n:int) =
                let a = System.Buffers.ArrayPool<Rune>.Shared.Rent(n)
                let rec loop i o =
                    if i = n then true
                    else
                        match Rune.DecodeFromUtf8(memory.Slice(o).Span) with
                        | (Buffers.OperationStatus.Done, rune, count) ->
                            a[i] <- rune
                            loop (i + 1) (o + count)
                        | _ -> false
                if loop 0 index then 
                    ValueSome (new ReadOnlyMemory<Rune>(a))
                else 
                    ValueNone

            member _.Next () : ValueOption<Rune * ByteRuneTextStream<'TState>> =
                match Rune.DecodeFromUtf8(memory.Slice(index).Span) with
                | (Buffers.OperationStatus.Done, rune, count) -> 
                    let l = if rune = newline then line + 1 else line
                    ValueSome (rune, ByteRuneTextStream<'TState>(state, memory, index + count, l))
                | _ -> ValueNone

            member _.Next (n:int) : ValueOption<ReadOnlyMemory<Rune> * ByteRuneTextStream<'TState>> =
                let a = System.Buffers.ArrayPool<Rune>.Shared.Rent(n)
                let rec loop i o l =
                    if i = n then 
                        true, o, l
                    else
                        match Rune.DecodeFromUtf8(memory.Slice(o).Span) with
                        | (Buffers.OperationStatus.Done, rune, count) ->
                            let l = if rune = newline then l + 1 else l
                            a[i] <- rune
                            loop (i + 1) (o + count) l
                        | _ -> 
                            false, 0, 0
                let (success, offset, line) = loop 0 index line
                if success then 
                    ValueSome (new ReadOnlyMemory<Rune>(a), ByteRuneTextStream<'TState>(state, memory, index + offset, line)) 
                else 
                    ValueNone

        type CharTextStream<'TState> (state: 'TState, memory:ReadOnlyMemory<char>, index: int, line: int, column: int) = 
                static let newline = '\n'

                static member Create<'TState> (state: 'TState, text:string) : ITextStream<'TState, char> =
                    if text = null then
                        nullArg (nameof(text))
                    let memory = text.ReplaceLineEndings("\n").AsMemory()              
                    CharTextStream<'TState>(state, memory, 0, 1, 1)

                interface ITextStream<'TState, char> with
                
                    member _.State = state

                    member _.Index = index

                    member _.Line = line

                    member _.Column = column

                    member _.Remaining = memory.Length - index

                    member _.IsComplete = index = memory.Length

                    member x.Value = (x:>ITextStream<'TState, char>).Peek()
                
                    member _.Peek () : ValueOption<char> =
                        if index < memory.Length then 
                            ValueSome (memory.Slice(index, 1).Span[0])
                        else 
                            ValueNone

                    member _.Peek (count:int) : ValueOption<ReadOnlyMemory<char>> =
                        if index + count <= memory.Length then 
                            ValueSome (memory.Slice(index, count))
                        else 
                            ValueNone

                    member _.Next () : ValueOption<char * ITextStream<'TState, char>> =
                        if index < memory.Length then 
                            let r = memory.Slice(index, 1).Span[0]
                            let l = if r = newline then line + 1 else line
                            ValueSome (r, CharTextStream<'TState>(state, memory, index + 1, l, 1))
                        else 
                            ValueNone

                    member _.Next (count:int) : ValueOption<ReadOnlyMemory<char> * ITextStream<'TState, char>> =
                        if index + count <= memory.Length then 
                            let r = memory.Slice(index, count)
                            let mutable l = 0
                            for r in r.Span do
                                if r = newline then
                                    l <- l + 1
                            ValueSome (r, CharTextStream<'TState>(state, memory, index + count, line + l, 1))
                        else 
                            ValueNone

        type TextStream<'TState> (state: 'TState, memory:ReadOnlyMemory<Rune>, index: int, line: int, column: int) = 

            static let newline = new Rune('\n')

            let getCount range remaining =
                match range with
                | One -> 1
                | Exact (Bounded count) -> count
                | Between (Bounded lower, Bounded upper) ->  max lower (min upper remaining)
                | Between (Unbounded, Bounded upper) ->  min upper remaining
                | Between (Bounded lower, Unbounded) ->  max lower remaining
                | Between (Unbounded, Unbounded) | Exact Unbounded | Remaining ->  remaining

            static member Create<'TState> (state: 'TState, text:string) =
                if text = null then
                    nullArg (nameof(text))
                let memory = ReadOnlyMemory(text.ReplaceLineEndings("\n").EnumerateRunes() |> Seq.toArray)                
                TextStream<'TState>(state, memory, 0, 1, 1)


            member _.State = state

            member _.Index = index

            member _.Line = line

            member _.Column = column

            member _.Remaining = memory.Length - index

            member _.IsComplete = index = memory.Length

            member x.Value = x.Peek()
                
            member _.Peek () : ValueOption<Rune> =
                if index < memory.Length then 
                    ValueSome (memory.Slice(index, 1).Span[0])
                else 
                    ValueNone

            member this.Peek (count:int) : ValueOption<ReadOnlyMemory<Rune>> = 
                this.Peek (Exact(Bounded count))  

            member this.Peek (range:Range<int>) : ValueOption<ReadOnlyMemory<Rune>> =
                let count = getCount range this.Remaining
                if index + count > memory.Length then
                    ValueNone
                else
                    ValueSome (memory.Slice(index, count))

            member _.Next () : ValueOption<Rune * TextStream<'TState>> =
                if index < memory.Length then 
                    let r = memory.Slice(index, 1).Span[0]
                    let l, c = if r = newline then line + 1, 1 else line, column + 1
                    ValueSome (r, TextStream<'TState>(state, memory, index + 1, l, c))
                else 
                    ValueNone

            member this.Next (count:int) : ValueOption<ReadOnlyMemory<Rune> * TextStream<'TState>> = 
                this.Next (Exact(Bounded count))  

            member this.Next (range:Range<int>) : ValueOption<ReadOnlyMemory<Rune> * TextStream<'TState>> =
                let count = getCount range this.Remaining
                if index + count > memory.Length then
                    ValueNone
                else
                    let r = memory.Slice(index, count)
                    let mutable l = line
                    let mutable c = column
                    for r in r.Span do
                        c <- c + 1
                        if r = newline then
                            c <- 1
                            l <- l + 1
                    ValueSome (r, TextStream<'TState>(state, memory, index + count, l, c))
                    
            member _.CreateFailure<'e, 'u when 'e :> exn> (data:'u) (f:'u * int * int * int -> 'e) =
                f (data, index, line, column)
    
    end

