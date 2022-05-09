namespace PureParse

#nowarn "9"

open System
open FSharp.NativeInterop
open System.Numerics;
open System.Runtime.Intrinsics;

module Parse1 = 

    [<Struct>]
    type State = {
        text : string
        index : int
    }

    type Result<'t> = 
        Success of State * 't 
        | Failure of State * string

    /// Parser Monad Shorthand
    type M<'t> = State -> Result<'t>

    /// Parser Monad
    type Parser<'t> = M<'t>
    
    /// Monad Transform Shorthand
    type F<'t, 'u> = 't -> M<'u>
    
    /// Monad Transform
    type Transform<'t, 'u> = F<'t, 'u>
    
    /// Monad Delay Shorthand
    type D<'t> = unit -> M<'t>
    
    /// Monad Delay
    type Delayed<'t> = unit -> M<'t>
    
    /// The bind signature of the parser monad
    type Bind<'a, 'b> = Parser<'a> -> Transform<'a, 'b> -> Parser<'b>
    
    /// The return signature of the parser monad
    type Return<'a> = 'a -> Parser<'a>
    
    /// The 'bind' function of the monad
    let bind (parser: Parser<_>) (transform: Transform<_, _>) state = 
        match parser state with
        | Success (state, value) -> 
            transform value state
        | Failure (_, message) -> 
            Failure (state, message)
            
    /// The 'bind' function of the monad
    let (>>=) p f s = bind p f s

    /// The 'pipe' function of the monad
    let (>>) p1 p2 state = p1 >>= (fun _ -> p2) state

    /// The 'return' or 'unit' function of the monad
    let result value state = 
        Success (state, value) 

    /// The 'return' or 'unit' function of the monad
    let fail message state = 
        Failure (state, message) 

    type ParseBuilder () = 

        member _.Bind (p:Parser<_>, f:Transform<_, _>):Parser<_> = 
            fun state -> bind p f state

        member _.Return (value):Parser<_> = 
            fun state -> result value state

        member _.Zero ():Parser<_> = 
            fun state -> result () state

        member _.ReturnFrom (p:Parser<_>):Parser<_> = 
            fun state -> p state

        member _.Delay (delayed:Delayed<_>) : Parser<_> = 
            fun state -> delayed () state

        member _.Run<'a> (parser:Parser<'a>) : Parser<'a> = 
            fun state -> parser state

    let parse = ParseBuilder()

    let inline matchStringAtIndex (text:string) (index:int) (test:string) = 
        text.IndexOf(test, index) = index
                                                       
    let inline getChar state = 
        state.text.[state.index]
             
    let parseChar c = 
        fun state ->
            if state.index < state.text.Length && getChar state = c 
            then Success ({ state with index = state.index + 1 }, c) 
            else Failure (state, "")  

    let parseString cs =
        fun state ->
            if state.index < state.text.Length && matchStringAtIndex state.text state.index cs
            then Success ({ state with index = state.index + cs.Length }, cs) 
            else Failure (state, "")   

    let skipChar c = 
        fun state ->
            if state.index < state.text.Length && getChar state = c 
            then Success ({ state with index = state.index + 1 }, ()) 
            else Failure (state, "") 

    let skipString cs =
        fun state ->
            if state.index < state.text.Length && matchStringAtIndex state.text state.index cs
            then Success ({ state with index = state.index + cs.Length }, ()) 
            else Failure (state, "")
         
    let parseOneOf cs = 
        let set = Set.ofSeq cs
        fun state -> 
            if state.index >= state.text.Length 
            then Failure (state, "")
            else 
                let c = getChar state
                if set.Contains c
                then Success ({ state with index = state.index + 1 }, c) 
                else Failure (state, "")

    /// This is a 'choice' combinator.
    let (<|>) p1 p2 state =
        match p1 state with
        | Success (_, _) as result -> result
        | Failure (state, _) -> p2 state

    /// This parser is always a success returning an Option value
    let optional (parser:M<_>) state = 
        match parser state with
        | Success (state, value) -> Success (state, Some value) 
        | Failure (_, _) -> Success (state, None)
        
    /// This parser is always a success returning an Option value. The state of the computation is unchanged.
    let peek (parser:M<_>) state = 
        match parser state with
        | Success (_, value) -> Success (state, Some value) 
        | Failure (_, _) -> Success (state, None)

    /// This parser is always a success returning an Option value
    let opt (parser:M<_>) state = optional parser state

    /// This is a 'choice' combinator.
    let choose<'a> (parsers:Parser<'a> list) state =
        match parsers with
        | [ p1; p2 ] -> (p1 <|> p2) state
        | _ ->
            let parse p  = 
                async { match p state with
                        | Success (_, _) as result -> 
                            return Some result
                        | Failure (_, _) -> 
                            return None }
            let a = 
                parsers 
                |> Seq.map parse 
                |> Async.Choice 
                |> Async.RunSynchronously
            match a with
            | Some n -> n
            | None -> Failure (state, "None of the choices were Success.")
            
    /// Pass over skip and evaluate parser.
    let skip (parser:M<_>) state =
        match parser state with
        | Success(state, _)
        | Failure(state, _) -> result () state

    /// Pass over skip and evaluate parser.
    let skipParse (skip:M<_>) (parser:M<_>) state =
        match skip state with
        | Success(state, _) -> parser state
        | Failure(state, _) -> parser state

    /// Parse 'p' until your reach the end parser 'e'
    let parseUntil<'a> (p:M<'a>) (e:M<unit>) state : Result<'a list> =
        let first = p state 
        match first with
        | Failure (state, message) -> 
            Failure (state, message)
        | Success (state, first) ->
            let a = ResizeArray<'a>()
            a.Add (first)
            let rec parse state =
                let inline complete () = a.ToArray() |> List.ofArray
                match e state with
                | Success (state, _) -> 
                    result (complete ()) state
                | Failure (state, _) -> 
                    match p state with
                    | Failure (state, message) -> 
                        Failure (state, message)
                    | Success (state, _) ->
                        parse state
            parse state
 
    let parseNoneOf value =
        let set = Set.ofSeq value
        fun state -> 
            if state.index >= state.text.Length 
            then Failure (state, "")
            else 
                let c = getChar state
                if set.Contains c
                then Failure (state, "")                    
                else Success ({ state with index = state.index + 1 }, c) 

    let sequence<'a> (parsers:Parser<'a> list) : Parser<'a list>  = 
        fun (state: State) ->
            let rec parse ps state rs =
                match ps with
                | [] -> Success (state, rs |> List.rev)
                | p::ps -> 
                    match p state with
                    | Success (state, v) ->
                        parse ps state (v::rs)
                    | Failure (_, message) ->
                        Failure (state, message)
            parse parsers state []

    let skipSequence<'a> (parsers:Parser<'a> list) : Parser<unit>  = 
        fun (state: State) ->
            let rec parse ps state =
                match ps with
                | [] -> Success (state, ())
                | p::ps -> 
                    match p state with
                    | Success (state, _) ->
                        parse ps state
                    | Failure (_, message) ->
                        Failure (state, message)
            parse parsers state        

    let parseList<'a> (parser:M<'a>) (separator:M<unit>) allowTrailingSeparator state :Result<'a list> =
        let first = parser state 
        match first with
        | Failure (state, message) -> 
            Failure (state, message)
        | Success (state, first) ->
            let a = ResizeArray<'a>()
            a.Add (first)
            let rec parse state =
                let inline complete () = a.ToArray() |> List.ofArray
                match separator state with
                | Failure (state, _) -> 
                    result (complete ()) state
                | Success (state, _) -> 
                    match parser state with
                    | Failure (state, message) -> 
                        if allowTrailingSeparator 
                        then result (complete ()) state
                        else Failure (state, message)
                    | Success (state, next) ->
                        a.Add(next)
                        parse state
            parse state
    
    let parseCharString (p:M<char>) state  =
        let i = state.index
        let rec run state (sb:System.Text.StringBuilder) =
            match p state with
            | Success (state, v) -> 
                sb.Append(v) |> ignore
                run state sb
            | Failure (state, _) -> state
        let sb = System.Text.StringBuilder()
        let nextState = run state sb
        let y = nextState.index
        if y = i 
        then Failure(state, "")
        else Success ({ state with index = y }, sb.ToString())            

    let parseCharSetString1 (cs:Set<char>) state  =
        let i = state.index
        let e = 
            [i..state.text.Length - 1] 
            |> Seq.skipWhile (fun n -> cs.Contains(state.text.[n])) 
            |> Seq.tryHead
        match e with
        | Some e -> 
            Success ({ state with index = e - 1}, state.text.[i..e - 1])
        | None ->
            Failure(state, "")

    let parseCharSetString (cs:Set<char>) state  =
        let i = state.index
        let e = state.text.Length - 1
        let remaining = e - i
        use xs = fixed state.text;
        let found i = cs.Contains(NativePtr.get xs i)
        let increment i = i + 1
        let result = 
            Seq.init remaining increment
            |> Seq.takeWhile found
            |> Seq.tryHead  
        match result with
        | Some e -> 
            Success ({ state with index = e - 1}, state.text.[i..e - 1])
        | None ->
            Failure(state, "")

    let private parseString2 (value:string) =
        let encoding = System.Text.Encoding.Default
        let bytes = encoding.GetBytes(value)
        let span = bytes.AsSpan()
        let vector = Vector<int64>(span)
        fun state ->
            let span = state.text.AsSpan(state.index, value.Length)
            let bs = NativePtr.stackalloc<byte> (span.Length * 2) |> NativePtr.toVoidPtr
            let s = Span(bs, span.Length * 2)
            encoding.GetBytes(span, s) |> ignore
            let vector2 = Vector<int64>(s)
            if Vector.EqualsAll<int64>(vector, vector2) 
            then Success({state with index = state.index + value.Length }, value)
            else Failure (state, "")

    let parseString3 (value:string) : Parser<string> =
        fun state ->
            let enoughLength = state.index < state.text.Length
            if enoughLength && state.text.AsSpan(state.index, value.Length).StartsWith(value.AsSpan())
            then Success ({ state with index = state.index + value.Length }, value)
            else Failure (state, sprintf "The string was not found at index %i." state.index)

