namespace PureParse

open System.Threading.Tasks
open System.Diagnostics

#nowarn "9"
open System
open System.Threading.Channels
open System.Text
open System.Buffers
open FSharp.NativeInterop
open System.Numerics;
open System.Runtime.Intrinsics;
open Events

[<AutoOpen>]
module TextStream = 
    begin

        let inline convertToMemory (text:string) =
            if text = null then nullArg (nameof(text))
            text.ReplaceLineEndings("\n").EnumerateRunes() |> Seq.toArray |> ReadOnlyMemory

        /// <summary>A stream of content.</summary>
        /// <typeparam name="TState">The custom user state for the stream.</typeparam>
        /// <typeparam name="TContent">The content of the stream..</typeparam>
        type ITextStream<'TState, 'TContent when 'TContent: comparison> = interface

            abstract member State : 'TState
            abstract member Index : int
            abstract member Line : int
            abstract member Column : int
            abstract member Remaining : int
            abstract member IsComplete : bool
            abstract member Value : ValueOption<'TContent>
            
            abstract member SetState : nextState: 'TState -> ITextStream<'TState, 'TContent>
            abstract member TransformState : transform: ('TState -> 'TState) -> ITextStream<'TState, 'TContent>

            abstract member Peek : unit -> ValueOption<'TContent>
            abstract member Peek : count : int -> ValueOption<ReadOnlyMemory<'TContent>>
            abstract member Peek : range : Range<int> -> ValueOption<ReadOnlyMemory<'TContent>>
            abstract member Peek : set : Set<'TContent> -> ValueOption<ReadOnlyMemory<'TContent>>
            abstract member Peek : predicate : ('TContent -> bool) -> ValueOption<ReadOnlyMemory<'TContent>>
            abstract member Peek : exactMatch : string -> ValueOption<ReadOnlyMemory<'TContent>>
            abstract member Peek : exactMatch : ReadOnlyMemory<'TContent> -> ValueOption<ReadOnlyMemory<'TContent>>

            abstract member Next : unit -> ValueOption<'TContent * ITextStream<'TState, 'TContent>>
            abstract member Next : count : int -> ValueOption<ReadOnlyMemory<'TContent> * ITextStream<'TState, 'TContent>>
            abstract member Next : range : Range<int> -> ValueOption<ReadOnlyMemory<'TContent> * ITextStream<'TState, 'TContent>>
            abstract member Next : set : Set<'TContent> -> ValueOption<ReadOnlyMemory<'TContent> * ITextStream<'TState, 'TContent>>
            abstract member Next : predicate : ('TContent -> bool) -> ValueOption<ReadOnlyMemory<'TContent> * ITextStream<'TState, 'TContent>>
            abstract member Next : exactMatch : string -> ValueOption<ReadOnlyMemory<'TContent> * ITextStream<'TState, 'TContent>>
            abstract member Next : exactMatch : ReadOnlyMemory<'TContent> -> ValueOption<ReadOnlyMemory<'TContent> * ITextStream<'TState, 'TContent>>
        end

        type TextStream<'TState> private (
                state: 'TState, 
                memory: ReadOnlyMemory<Rune>, 
                eventChannel: Channel<Event<'TState>>,
                eventTreeTask: Task<EventTree<'TState>>,
                index: int, 
                line: int, 
                column: int) = 

            static let newline = new Rune('\n')
            static let errorMessageDefault = ""

            let getCount range remaining =
                match range with                
                | Exact (Bounded count) when count < 0 -> 
                    raise <| ArgumentOutOfRangeException(nameof(range), "Must be at least 1.")
                | Exact (Bounded count) -> count

                | Between (Bounded(lower), Unbounded) when lower < 0 ->
                    raise <| ArgumentOutOfRangeException(nameof(range), "The lower bound must be at least 1.")
                | Between (Bounded(lower), Unbounded) -> max lower remaining

                | Between (Unbounded, Bounded(upper)) when upper < 0 ->
                    raise <| ArgumentOutOfRangeException(nameof(range), "The upper bound must be at least 1.")
                | Between (Unbounded, Bounded(upper)) -> min upper remaining

                | Between (Bounded(lower), Bounded(upper)) when lower < 0 || upper < 0 ->
                    raise <| ArgumentOutOfRangeException(nameof(range), "The bounds must be at least 1.")
                | Between (Bounded(lower), Bounded(upper)) -> max lower (min upper remaining)
                
                | Exact Unbounded -> remaining
                | Between (Unbounded, Unbounded) -> remaining
                | Remaining -> remaining

            let findLastIndex (predicate:(Rune -> bool)) =
                let span = memory.Slice(index).Span
                let mutable i = 0
                while i < span.Length && 
                        predicate(span[i]) do 
                            i <- i + 1
                i

            let calculateLineAndColumn (m:ReadOnlyMemory<Rune>) = 
                let mutable l = line
                let mutable c = column
                for r in m.Span do
                    c <- c + 1
                    if r = newline then
                        c <- 1
                        l <- l + 1
                l, c

            static member Create<'TState> (state: 'TState, text:string) =
                let memory = convertToMemory text
                let eventChannel = Channel.CreateUnbounded<Event<'TState>>()
                let eventTreeTask = Events.createEventTreeBuilder text eventChannel
                TextStream<'TState>(state, memory, eventChannel, eventTreeTask, 0, 1, 1)

            
            [<Conditional("DEBUG")>]
            member _.Assertions () =
                assert(line > 0)
                assert(column > 0)
                assert(index >= 0 && index <= memory.Length)

            /// After the parse operation is complete, invoke this method to wait for the event tree.
            member _.GetEventTree () =
                eventTreeTask.Wait()
                eventTreeTask.Result                

            member _.ReportEvent (event:Event<_>) =
                eventChannel.Writer.WriteAsync(event) |> ignore  
                
            member this.CreateErrorEventData (parserName, ?message) =
                let message = Option.defaultValue errorMessageDefault message
                this.CreateEventData(parserName, message, ParseError(message, index, line, column))

            member _.CreateEventData (parserName, ?message, ?error:exn) =
                if parserName = null then nullArg (nameof(parserName))
                {
                    parserName = parserName
                    message = Option.defaultValue errorMessageDefault message
                    index = index
                    line = line
                    column = column
                    timestamp = getTimeStamp ()
                    state = state
                    error = error
                }

            member _.State = state

            member _.Index = index

            member _.Line = line

            member _.Column = column

            member _.Remaining = memory.Length - index

            member _.IsComplete = index = memory.Length

            member x.Value = x.Peek()
            
            member _.SetState nextState =
                TextStream<'TState>(nextState, memory, eventChannel, eventTreeTask, index, line, column)

            member _.TransformState transform =
                TextStream<'TState>(transform state, memory, eventChannel, eventTreeTask, index, line, column)
                
            member _.Peek () : ValueOption<Rune> =
                if index < memory.Length then 
                    ValueSome (memory.Slice(index, 1).Span[0])
                else 
                    ValueNone

            member this.Peek (count:int) : ValueOption<ReadOnlyMemory<Rune>> = 
                if count <= 0 then 
                    raise <| ArgumentOutOfRangeException(nameof(count), "Must be at least 1.")
                this.Peek (Exact (Bounded count))  

            member this.Peek (range:Range<int>) : ValueOption<ReadOnlyMemory<Rune>> =
                let count = getCount range this.Remaining
                if index + count > memory.Length then
                    ValueNone
                else
                    ValueSome (memory.Slice(index, count))

            member this.Peek (set:Set<Rune>) : ValueOption<ReadOnlyMemory<Rune>> = 
                this.Assertions()
                if set.IsEmpty then 
                    invalidArg (nameof(set)) "At least 1 rune is required."
                this.Peek(set.Contains)

            member this.Peek (predicate:(Rune -> bool)) : ValueOption<ReadOnlyMemory<Rune>> = 
                this.Assertions()
                if this.Remaining = 0 then
                    ValueNone
                else
                    let i = findLastIndex predicate
                    if i = 0 then
                        ValueNone
                    else
                        ValueSome(memory.Slice(index, index + i - index))

            member this.Peek (exactMatch:string) : ValueOption<ReadOnlyMemory<Rune>> =
                this.Assertions()
                if exactMatch = null then nullArg (nameof(exactMatch))
                let runes = exactMatch.EnumerateRunes() |> Seq.toArray |> ReadOnlyMemory
                this.Peek(runes)

            member this.Peek (exactMatch:ReadOnlyMemory<Rune>) : ValueOption<ReadOnlyMemory<Rune>> =
                this.Assertions()
                if exactMatch.Length > this.Remaining then
                    ValueNone
                else
                    let data = memory.Slice(index, exactMatch.Length)
                    if not <| data.Span.SequenceEqual(exactMatch.Span) then
                        ValueNone
                    else
                        ValueSome(data)  

            member _.Next () : ValueOption<Rune * TextStream<'TState>> =
                if index < memory.Length then 
                    let r = memory.Slice(index, 1).Span[0]
                    let l, c = if r = newline then line + 1, 1 else line, column + 1
                    ValueSome (r, TextStream<'TState>(state, memory, eventChannel, eventTreeTask, index + 1, l, c))
                else 
                    ValueNone

            member this.Next (count:int) : ValueOption<ReadOnlyMemory<Rune> * TextStream<'TState>> = 
                if count <= 0 then 
                    raise <| ArgumentOutOfRangeException(nameof(count), "Must be at least 1.")
                this.Next (Exact (Bounded count)) 

            member this.Next (range:Range<int>) : ValueOption<ReadOnlyMemory<Rune> * TextStream<'TState>> =
                let count = getCount range this.Remaining
                if index + count > memory.Length then
                    ValueNone
                else
                    let r = memory.Slice(index, count)
                    let l, c = calculateLineAndColumn r
                    ValueSome (r, TextStream<'TState>(state, memory, eventChannel, eventTreeTask, index + count, l, c))

            member this.Next (set:Set<Rune>) : ValueOption<ReadOnlyMemory<Rune> * TextStream<'TState>> = 
                this.Assertions()
                if set.IsEmpty then 
                    invalidArg (nameof(set)) "At least 1 rune is required."
                this.Next(set.Contains)
                    
            member this.Next (predicate:(Rune -> bool)) : ValueOption<ReadOnlyMemory<Rune> * TextStream<'TState>> = 
                this.Assertions()
                if this.Remaining = 0 then
                    ValueNone
                else
                    let i = findLastIndex predicate
                    if i = 0 then
                        ValueNone
                    else
                        let result = memory.Slice(index, index + i - index)
                        let l, c = calculateLineAndColumn result
                        ValueSome(result, TextStream<'TState>(state, memory, eventChannel, eventTreeTask, index + i, l, c))

            member this.Next (exactMatch:string) : ValueOption<ReadOnlyMemory<Rune> * TextStream<'TState>> =
                this.Assertions()
                this.Next(convertToMemory exactMatch)

            member this.Next (exactMatch:ReadOnlyMemory<Rune>) : ValueOption<ReadOnlyMemory<Rune> * TextStream<'TState>> =
                this.Assertions()
                if exactMatch.Length > this.Remaining then
                    ValueNone
                else
                    let data = memory.Slice(index, exactMatch.Length)
                    let span = data.Span
                    if not <| span.SequenceEqual(exactMatch.Span) then
                        ValueNone
                    else
                        let l, c = calculateLineAndColumn data
                        ValueSome(data, TextStream<'TState>(state, memory, eventChannel, eventTreeTask, index + exactMatch.Length, l, c))


            interface ITextStream<'TState, Rune> with

                member this.State = 
                    (this :> ITextStream<'TState, Rune>).State

                member this.Index = 
                    (this :> ITextStream<'TState, Rune>).Index

                member this.Line = 
                    (this :> ITextStream<'TState, Rune>).Line

                member this.Column = 
                    (this :> ITextStream<'TState, Rune>).Column

                member this.Remaining = 
                    (this :> ITextStream<'TState, Rune>).Remaining

                member this.IsComplete = 
                    (this :> ITextStream<'TState, Rune>).IsComplete

                member this.Value = 
                    (this :> ITextStream<'TState, Rune>).Value
            
                member this.SetState (nextState: 'TState) = 
                    (this :> ITextStream<'TState, Rune>).SetState nextState

                member this.TransformState (transform: ('TState -> 'TState))  = 
                    (this :> ITextStream<'TState, Rune>).TransformState transform

                member this.Peek () = 
                    (this :> ITextStream<'TState, Rune>).Peek ()

                member this.Peek (count:int) = 
                    (this :> ITextStream<'TState, Rune>).Peek(count)

                member this.Peek (range : Range<int>) = 
                    (this :> ITextStream<'TState, Rune>).Peek range

                member this.Peek (set : Set<Rune>) = 
                    (this :> ITextStream<'TState, Rune>).Peek set

                member this.Peek (predicate : (Rune -> bool)) = 
                    (this :> ITextStream<'TState, Rune>).Peek predicate

                member this.Peek (exactMatch : string) = 
                    (this :> ITextStream<'TState, Rune>).Peek exactMatch

                member this.Peek (exactMatch : ReadOnlyMemory<Rune>) = 
                    (this :> ITextStream<'TState, Rune>).Peek exactMatch
                    
                member this.Next () =  
                    (this :> ITextStream<'TState, Rune>).Next()

                member this.Next (count:int) = 
                    (this :> ITextStream<'TState, Rune>).Next(count)

                member this.Next (range : Range<int>) = 
                    (this :> ITextStream<'TState, Rune>).Next(range)

                member this.Next (set : Set<Rune>) = 
                    (this :> ITextStream<'TState, Rune>).Next(set)

                member this.Next (predicate : (Rune -> bool)) = 
                    (this :> ITextStream<'TState, Rune>).Next(predicate)

                member this.Next (exactMatch : string) = 
                    (this :> ITextStream<'TState, Rune>).Next(exactMatch)

                member this.Next (exactMatch : ReadOnlyMemory<Rune>) = 
                    (this :> ITextStream<'TState, Rune>).Next(exactMatch)
    end