namespace PureParse

open System
open System.Text
open System.Numerics
open System.Globalization

[<AutoOpen>]
module NumberParsers =    
            
    let dot = Set.ofList [ Rune '.'; ]
    let signs = Set.ofList [ Rune '-'; Rune '+'; ]
    let exponents = Set.ofList [ Rune 'e'; Rune 'E'; ]

    /// Calculates all base-10 power values greater or equal to zero.
    type Base10PowerFunction<'TNumber when 'TNumber :> INumberBase<'TNumber>>() =
        static let ten = 'TNumber.Parse("10", CultureInfo.InvariantCulture)
        static member Calculate (n:int) = 
            if n < 0 then
                raise (ArgumentOutOfRangeException(nameof n, "Value cannot be less than zero."))
            let mutable x = 'TNumber.One
            for _ = 0 to n - 1 do 
                x <- x * ten
            x

    /// Parse a valid Int32 number.
    let parseInt32<'TState> () : Parser<'TState, int> =
        let digitToInt = function
            | x when x >= '0' && x <= '9' -> int x - int '0'
            | x when x >= 'a' && x <= 'f' -> 10 + int x - int 'a'
            | x when x >= 'A' && x <= 'F' -> 10 + int x - int 'A'
            | x -> failwith $"The char '{x}' is not expected."
        let rec parseInteger = function
            | [] -> 0.0
            | c::cs ->
                let d = digitToInt c |> double
                let e = (10.0 ** double cs.Length)
                d * e + parseInteger cs
        fun (stream:TextStream<'TState>) ->
            let sign, signStream =
                match stream.Next("-") with
                | ValueSome (_, s) -> -1, s
                | _ -> 1, stream
            match signStream.Next(Rune.IsDigit) with
            | ValueNone -> 
                stream.ReportEvent(ParseFailure(stream.CreateErrorEventData("Int32", $"No Characters Remaining")))
                Failure (stream)
            | ValueSome (rm:ReadOnlyMemory<Rune>, stream) -> 
                let s = rm.Span
                let mutable i = 0
                for x = 0 to s.Length - 1 do
                    let c = char (s[x].Value)
                    let h = (digitToInt c) |> double
                    let remaining = (s.Length - (x + 1)) |> double
                    i <- i + int(h * (10.0 ** remaining))
                Success (stream, sign * i)

    /// Parse a valid integer using any valid Signed Number type.
    let parseInteger<'TState, 'TNumber when 'TNumber :> ISignedNumber<'TNumber>> () : Parser<'TState, 'TNumber> =
        fun (stream:TextStream<'TState>) ->  
            let sign, signStream =
                match stream.Next(signs) with
                | ValueSome (Runes "-", s) -> 'TNumber.NegativeOne, s
                | ValueSome (Runes "+", s) -> 'TNumber.One, s
                | _ -> 'TNumber.One, stream
            match signStream.Next(Rune.IsDigit) with
            | ValueNone -> 
                Failure (stream)
            | ValueSome (rm:ReadOnlyMemory<Rune>, integerStream) -> 
                let s = rm.Span
                let mutable i = 'TNumber.Zero
                for x = 0 to s.Length - 1 do
                    let h = 'TNumber.CreateChecked(s[x].Value - int '0')
                    let r = Base10PowerFunction<'TNumber>.Calculate (s.Length - (x + 1))
                    i <- i + (h * r)
                Success(integerStream, sign * i)

    let parseFloat<'TState> () : Parser<'TState, double> =
        fun (stream:TextStream<'TState>) ->
            // integer
            let sign, signStream =
                match stream.Next(signs) with
                | ValueSome (Runes "-", s) -> -1.0, s
                | ValueSome (Runes "+", s) -> 1.0, s
                | _ -> 1.0, stream
            match signStream.Next(Rune.IsDigit) with
            | ValueNone -> 
                stream.ReportEvent(ParseFailure(stream.CreateErrorEventData("Int32", $"No Characters Remaining")))
                Failure (stream)
            | ValueSome (rm:ReadOnlyMemory<Rune>, integerStream) -> 
                let s = rm.Span
                let mutable i = 0
                for x = 0 to s.Length - 1 do
                    let h = double(s[x].Value - int '0')
                    let remaining = (s.Length - (x + 1)) |> double
                    i <- i + int(h * (10.0 ** remaining)) 
                let i = double i

                // decimal
                let struct(d, decimalStream, decimalInvalid) =
                    match integerStream.Next(dot) with
                    | ValueSome (_, s) ->
                        match s.Next(Rune.IsDigit) with
                        | ValueNone -> struct(0.0, stream, true)
                        | ValueSome (rm:ReadOnlyMemory<Rune>, decimalStream) -> 
                            let s = rm.Span
                            let mutable n = 10.0
                            let mutable d = 0.0
                            for x = 0 to s.Length - 1 do
                                let h = double(s[x].Value - int '0')
                                d <- d + (h / n)   
                                n <- n * 10.0
                            struct(d, decimalStream, false)
                    | _ ->  
                        struct(0.0, integerStream, false)
                
                if decimalInvalid then
                    Failure(stream)
                else 
                    // exponent
                    let struct(e, exponentStream, exponentInvalid) =
                        match decimalStream.Next(exponents) with
                        | ValueNone -> struct(0.0, decimalStream, false)
                        | ValueSome (_, exponentStream) -> 
                            let exponentSign, signsStream = 
                                match exponentStream.Next(signs) with
                                | ValueSome (RuneString "-", signsStream) -> -1.0, signsStream 
                                | ValueSome (RuneString "+", signsStream) -> 1.0, signsStream 
                                | _ -> 1.0, exponentStream
                            match signsStream.Next(Rune.IsDigit) with
                            | ValueNone ->  struct(0.0, stream, true)
                            | ValueSome (rm:ReadOnlyMemory<Rune>, exponentStream) -> 
                                let s = rm.Span
                                let mutable e = 0
                                for x = 0 to s.Length - 1 do
                                    let h = double(s[x].Value - int '0')
                                    let remaining = (s.Length - (x + 1)) |> double
                                    e <- e + int(h * (10.0 ** remaining)) 
                                let e = double e
                                let e = e * exponentSign
                                struct(e, exponentStream, false)
                    if exponentInvalid then
                        Failure(stream)
                    else
                        let n = ((i + d) * (10.0 ** e))
                        let d1 = Math.IEEERemainder(n, 1.0)
                        let d2 = Math.Round(d1, 10)
                        let diff = abs (d1 - d2)
                        let n = if diff < 0.00000000001 then n - diff else n
                        Success (exponentStream, sign * n)

    let parseDecimal<'TState> () : Parser<'TState, decimal> =
        fun (stream:TextStream<'TState>) ->
            // integer
            let sign, signStream =
                match stream.Next(signs) with
                | ValueSome (Runes "-", s) -> -1m, s
                | ValueSome (Runes "+", s) -> 1m, s
                | _ -> 1m, stream
            match signStream.Next(Rune.IsDigit) with
            | ValueNone -> 
                stream.ReportEvent(ParseFailure(stream.CreateErrorEventData("Int32", $"No Characters Remaining")))
                Failure (stream)
            | ValueSome (rm:ReadOnlyMemory<Rune>, integerStream) -> 
                let s = rm.Span
                let mutable i = 0m
                for x = 0 to s.Length - 1 do
                    let h = decimal(s[x].Value - int '0')
                    let r = decimal(Math.Pow(10.0, double(s.Length - (x + 1))))
                    i <- i + decimal(h * r) 
                let i = i

                // decimal
                let struct(d, decimalStream, decimalInvalid) =
                    match integerStream.Next(dot) with
                    | ValueSome (_, s) ->
                        match s.Next(Rune.IsDigit) with
                        | ValueNone -> struct(0m, stream, true)
                        | ValueSome (rm:ReadOnlyMemory<Rune>, decimalStream) -> 
                            let s = rm.Span
                            let mutable n = 10m
                            let mutable d = 0m
                            for x = 0 to s.Length - 1 do
                                let h = decimal(s[x].Value - int '0')
                                d <- d + (h / n)   
                                n <- n * 10m
                            struct(d, decimalStream, false)
                    | _ ->  
                        struct(0m, integerStream, false)
                
                if decimalInvalid then
                    Failure(stream)
                else 
                    // exponent
                    let struct(e, exponentStream, exponentInvalid) =
                        match decimalStream.Next(exponents) with
                        | ValueNone -> struct(0m, decimalStream, false)
                        | ValueSome (_, exponentStream) -> 
                            let exponentSign, signsStream = 
                                match exponentStream.Next(signs) with
                                | ValueSome (RuneString "-", signsStream) -> -1m, signsStream 
                                | ValueSome (RuneString "+", signsStream) -> 1m, signsStream 
                                | _ -> 1m, exponentStream
                            match signsStream.Next(Rune.IsDigit) with
                            | ValueNone ->  struct(0m, stream, true)
                            | ValueSome (rm:ReadOnlyMemory<Rune>, exponentStream) -> 
                                let s = rm.Span
                                let mutable e = 0m
                                for x = 0 to s.Length - 1 do
                                    let h = decimal(s[x].Value - int '0')
                                    let remaining = double(s.Length - (x + 1))
                                    let r = decimal(Math.Pow(10, remaining))
                                    e <- e + decimal(h * r) 
                                let e = decimal e
                                let e = e * exponentSign
                                struct(e, exponentStream, false)
                    if exponentInvalid then
                        Failure(stream)
                    else
                        let e = decimal (Math.Pow(10.0, double e))
                        let n = ((i + d) * e)
                        Success (exponentStream, sign * n)
    
    let inline private parseRealInteger<'TState, 'TNumber when 'TNumber :> System.Numerics.IFloatingPoint<'TNumber>> () : Parser<'TState, 'TNumber * 'TNumber> =
        fun (stream:TextStream<'TState>) ->  
            let sign, signStream =
                match stream.Next(signs) with
                | ValueSome (Runes "-", s) -> 'TNumber.NegativeOne, s
                | ValueSome (Runes "+", s) -> 'TNumber.One, s
                | _ -> 'TNumber.One, stream
            match signStream.Next(Rune.IsDigit) with
            | ValueNone -> 
                Failure (stream)
            | ValueSome (rm:ReadOnlyMemory<Rune>, integerStream) -> 
                let s = rm.Span
                let mutable i = 'TNumber.Zero
                for x = 0 to s.Length - 1 do
                    let h = 'TNumber.CreateChecked(s[x].Value - int '0')
                    let r = 'TNumber.CreateChecked(Math.Pow(10.0, double(s.Length - (x + 1))))
                    i <- i + (h * r)
                Success(integerStream, (sign, i))

    let inline private parseRealDecimal<'TState, 'TNumber when 'TNumber :> System.Numerics.IFloatingPoint<'TNumber>> () : Parser<'TState, 'TNumber> =  
        let ten = 'TNumber.Parse("10", System.Globalization.CultureInfo.InvariantCulture) 
        fun (stream:TextStream<'TState>) ->
            match stream.Next(dot) with
            | ValueSome (_, s) ->
                match s.Next(Rune.IsDigit) with
                | ValueNone -> PureParse.Result<'TState, 'TNumber>.Failure(stream)
                | ValueSome (rm:ReadOnlyMemory<Rune>, decimalStream) -> 
                    let s = rm.Span
                    let mutable n = ten
                    let mutable d = 'TNumber.Zero
                    for x = 0 to s.Length - 1 do
                        let h = 'TNumber.CreateChecked(s[x].Value - int '0')
                        d <- d + (h / n)   
                        n <- n * ten
                    Success(decimalStream, d)
            | _ ->  
                Success(stream, 'TNumber.Zero)

    let inline private parseRealExponent<'TState, 'TNumber when 'TNumber :> System.Numerics.IFloatingPoint<'TNumber>> () : Parser<'TState, 'TNumber> = 
        fun (stream:TextStream<'TState>) ->
            match stream.Next(exponents) with
            | ValueNone -> Success(stream, 'TNumber.One)
            | ValueSome (_, exponentStream) -> 
                let exponentSign, signsStream = 
                    match exponentStream.Next(signs) with
                    | ValueSome (RuneString "-", signsStream) -> 'TNumber.NegativeOne, signsStream 
                    | ValueSome (RuneString "+", signsStream) -> 'TNumber.One, signsStream 
                    | _ -> 'TNumber.One, exponentStream
                match signsStream.Next(Rune.IsDigit) with
                | ValueNone ->  Failure(stream)
                | ValueSome (rm:ReadOnlyMemory<Rune>, exponentStream) -> 
                    let s = rm.Span
                    let mutable e = 'TNumber.Zero
                    for x = 0 to s.Length - 1 do
                        let h = 'TNumber.CreateChecked(s[x].Value - int '0')
                        let r = 'TNumber.CreateChecked(Math.Pow(10.0, double(s.Length - (x + 1))))
                        e <- e + 'TNumber.CreateChecked(h * r) 
                    let e = e * exponentSign
                    let e = 'TNumber.CreateChecked (Math.Pow(10.0, Double.CreateChecked e))
                    Success(exponentStream, e)
    
    let inline private round (x: ^d) : ^R
        when ^d :> IBinaryFloatingPointIeee754<^d>
        and ^R :> IFloatingPoint<^R>
        =
            let d1 = 'd.Ieee754Remainder(x, 'd.One)
            let d2 = 'd.Round(d1, 10)
            let diff = 'd.Abs (d1 - d2)
            let z = if diff > 'd.Zero then x - diff else x
            'R.CreateChecked z
    
    let inline parseReal<'TState, 'TNumber when 'TNumber :> IFloatingPoint<'TNumber>> () : Parser<'TState, 'TNumber> =
        fun (stream:TextStream<'TState>) ->                    
            match parseRealInteger () stream with
            | Success (integerStream, (sign, i)) ->
                match parseRealDecimal () integerStream with
                | Success (parseRealDecimalStream, d) ->
                    match parseRealExponent () parseRealDecimalStream with
                    | Success (parseRealExponentStream, e) -> 
                        let n:'TNumber = 
                            match box ((i + d) * e) with
                            | :? Half as x -> round x
                            | :? Single as x -> round x
                            | :? Double as x -> round x 
                            | x -> unbox x
                        Success (parseRealExponentStream, sign * n)
                    | Failure (_) ->
                        Failure(stream)
                | Failure (_) ->
                    Failure(stream)
            | Failure (_) ->
                Failure(stream)

