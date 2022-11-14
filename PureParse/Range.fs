namespace PureParse
open System.Numerics

[<Struct>]
type Num<'n when 'n :> INumber<'n>> =
    | Unbounded
    | Bounded of 'n

[<Struct>]
type Range<'n when 'n :> INumber<'n>> =

    /// Give the exact amount specified
    | Exact of exact: Num<'n>

    /// Give anything in between the min and max values provided.
    | Between of min: Num<'n> * max: Num<'n>

    /// Give everything that remains.
    | Remaining