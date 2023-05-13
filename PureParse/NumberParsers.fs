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

    /// Parse a signed integer.
    let parseInteger<'TState, 'TNumber when 'TNumber :> ISignedNumber<'TNumber>> () : Parser<'TState, 'TNumber> =
        fun (stream:TextStream<'TState>) ->  
            let sign, signStream =
                match stream.Next() with
                | ValueSome (r, s) when char r.Value = '-' -> 'TNumber.NegativeOne, s
                | ValueSome (r, s) when char r.Value = '+' -> 'TNumber.One, s
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

    /// Parse an unsigned integer.
    let parseUnsignedInteger<'TState, 'TNumber when 'TNumber :> IUnsignedNumber<'TNumber>> () : Parser<'TState, 'TNumber> =
        fun (stream:TextStream<'TState>) ->  
            match stream.Next(Rune.IsDigit) with
            | ValueNone -> 
                Failure (stream)
            | ValueSome (rm:ReadOnlyMemory<Rune>, integerStream) -> 
                let s = rm.Span
                let mutable i = 'TNumber.Zero
                for x = 0 to s.Length - 1 do
                    let h = 'TNumber.CreateChecked(s[x].Value - int '0')
                    let r = Base10PowerFunction<'TNumber>.Calculate (s.Length - (x + 1))
                    i <- i + (h * r)
                Success(integerStream, i)
    
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
    
    let inline private round (x: ^d) (digits:int) : ^R
        when ^d :> IBinaryFloatingPointIeee754<^d>
        and ^R :> IFloatingPoint<^R>
        =
            let d1 = 'd.Ieee754Remainder(x, 'd.One)
            let d2 = 'd.Round(d1, digits)
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
                            | :? Half as x -> round x 5
                            | :? Single as x -> round x 6
                            | :? Double as x -> round x 10 
                            | x -> unbox x
                        Success (parseRealExponentStream, sign * n)
                    | Failure (_) ->
                        Failure(stream)
                | Failure (_) ->
                    Failure(stream)
            | Failure (_) ->
                Failure(stream)

