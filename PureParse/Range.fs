namespace PureParse

open System.Numerics

/// A numeric value that is limited to the INumber interface.
/// The Bounded and Unbounded types allow specifying infinity using any
/// of the numeric types.
[<Struct>]
type Num<'n when 'n :> INumber<'n>> =
    /// The number is not bounded to a specific value.
    | Unbounded
    /// The exact number provided.
    | Bounded of number:'n

/// Allows you to specify the range of values from a source.
[<Struct>]
type Range<'n when 'n :> INumber<'n>> =
    /// Give the exact amount specified
    | Exact of exact: Num<'n>
    /// Give anything in between the min and max values provided.
    | Between of min: Num<'n> * max: Num<'n>
    /// Give everything that remains.
    | Remaining