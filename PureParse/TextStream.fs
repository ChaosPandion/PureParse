namespace PureParse
#nowarn "9"
open System
open System.Text
open System.Buffers
open FSharp.NativeInterop
open System.Numerics;
open System.Runtime.Intrinsics;
open Events

[<AutoOpen>]
module TextStream = 
    begin

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

        type TextStream<'TState> (
                state: 'TState, 
                memory: ReadOnlyMemory<Rune>, 
                eventChannel: MailboxProcessor<Event<'TState>>,
                index: int, 
                line: int, 
                column: int) = 

            static let newline = new Rune('\n')

            let getCount range remaining =
                match range with
                | One -> 1
                | Exact (Bounded count) -> count
                | Between (Bounded lower, Bounded upper) ->  max lower (min upper remaining)
                | Between (Unbounded, Bounded upper) ->  min upper remaining
                | Between (Bounded lower, Unbounded) ->  max lower remaining
                | Between (Unbounded, Unbounded) | Exact Unbounded | Remaining ->  remaining

            static member Create<'TState> (state: 'TState, text:string, accept:EventTree.Accept<'TState>) =
                if text = null then
                    nullArg (nameof(text))
                let text = text.ReplaceLineEndings("\n")
                let memory = ReadOnlyMemory(text.EnumerateRunes() |> Seq.toArray)
                let mailbox = Events.createEventTreeBuilder(text, accept)
                TextStream<'TState>(state, memory, mailbox, 0, 1, 1), mailbox

                
            member _.ReportEvent (event:Event<_>) =
                eventChannel.Post event
                 
            member _.CreateEventData (parserName, message) = {
                        parserName = parserName
                        message = message
                        index = index
                        line = line
                        column = column
                        timestamp = getTimeStamp ()
                        state = state
                        error = exn()
                    }
 
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
                    ValueSome (r, TextStream<'TState>(state, memory, eventChannel, index + 1, l, c))
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
                    ValueSome (r, TextStream<'TState>(state, memory, eventChannel, index + count, l, c))
                    
            member _.CreateFailure<'e, 'u when 'e :> exn> (data:'u) (f:'u * int * int * int -> 'e) =
                f (data, index, line, column)
    
    end

